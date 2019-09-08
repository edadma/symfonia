package xyz.hyperreal.symfonia

import java.nio.FloatBuffer
import java.util.ServiceLoader
import java.util.logging.Level
import java.util.logging.Logger
import java.util.{List => JList}

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Sink, Source}
import akka.testkit.TestProbe

import org.jaudiolibs.audioservers.AudioClient
import org.jaudiolibs.audioservers.AudioConfiguration
import org.jaudiolibs.audioservers.AudioServerProvider
import org.jaudiolibs.audioservers.ext.ClientID
import org.jaudiolibs.audioservers.ext.Connections


object Player {

  lazy val loader = ServiceLoader.load(classOf[AudioServerProvider])

  def findProvider: Option[AudioServerProvider] = {
    for (p <- loader.iterator.asScala) {
      if (p.getLibraryName == "JACK")
        return Some( p )
    }

    None
  }

  lazy val provider =
    findProvider match {
      case Some( p ) => p
      case None => sys.error( "jack not found" )
    }

  def apply( src: Source[Double, _] ) = new PlayerClient( src )

  class PlayerClient( src: Source[Double, _] ) extends AudioClient {
    abstract class AckingReceiverMessage
    case object Ack extends AckingReceiverMessage
    case object StreamInitialized extends AckingReceiverMessage
    case object StreamCompleted extends AckingReceiverMessage
    case class StreamFailure( ex: Throwable ) extends AckingReceiverMessage

    val probe = TestProbe()
    val receiver = system.actorOf(Props(new AckingReceiver(probe.ref, ackWith = Ack)))
    val sink = Sink.actorRefWithAck(
      receiver,
      onInitMessage = StreamInitialized,
      ackMessage = Ack,
      onCompleteMessage = StreamCompleted,
      onFailureMessage = StreamFailure )
    val data = new mutable.Queue[Float]( 1024 )
    val sink = src map (_.toFloat) grouped 1024 buffer (1, OverflowStrategy.backpressure) runWith sink
    val config = new AudioConfiguration(
      44100.0f, //sample rate
      0, // input channels
      2, // output channels
      256, //buffer size
      // extensions
      new ClientID("Symfonia"),
      Connections.OUTPUT)

    val server = provider.createServer( config, this )

    val runner = new Thread( new Runnable {
      def run = {
        try {
          server.run
        } catch {
          case ex: Exception =>
            Logger.getLogger(this.getClass.getName).log(Level.SEVERE, null, ex)
        }
      }
    } )

    runner.setPriority( Thread.MAX_PRIORITY )
    runner.start

    var buffer: Array[Float] = _

    def join = runner.join

    def configure( context: AudioConfiguration ): Unit = {
      if (context.getOutputChannelCount != 2)
        sys.error( "only work with stereo output" )
    }

    var done = false

    def process( time: Long, inputs: JList[FloatBuffer], outputs: JList[FloatBuffer], nframes: Int ): Boolean = {
      if (done)
        return false

      val left = outputs.get(0)
      val right = outputs.get(1)

      if (buffer == null || buffer.length != nframes) {
        buffer = new Array[Float]( nframes )
      }

      val dequeued = data.length min nframes

      for (i <- 0 until dequeued)
        buffer(i) = data.dequeue

      if (dequeued < nframes)
        Await.result( sink.pull, Duration.Inf ) match {
          case Some( s ) =>
            for (i <- dequeued until nframes)
              buffer(i) = s(i - dequeued)

            for (i <- nframes - dequeued until s.length)
              data enqueue s(i)
          case None =>
            done = true
        }

      left.put(buffer)
      right.put(buffer)

      true
    }

    def shutdown = {

    }

    class AckingReceiver(probe: ActorRef, ackWith: Any) extends Actor with ActorLogging {
      def receive: Receive = {
        case StreamInitialized =>
          log.info("Stream initialized!")
          sender ! Ack
          data.clear
        case el: String =>
          log.info("Received element: {}", el)
          probe ! el
          sender ! Ack // ack to allow the stream to proceed sending more elements
        case StreamCompleted =>
          log.info("Stream completed!")
          context.system.terminate
        case StreamFailure(ex) =>
          log.error(ex, "Stream failed!")
      }
    }
  }

}