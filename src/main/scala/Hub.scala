package xyz.hyperreal.symfonia

import akka.stream.scaladsl.{Sink, SinkQueueWithCancel, Source}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object Hub {

  private def iterator( hub: Hub ) =
    new Iterator[Double] {
      var prev: Option[Double] = _

      def hasNext = {
        if (prev eq null)
          prev = hub.pull

        prev nonEmpty
      }

      def next = {
        if (hasNext) {
          val res = prev.get

          prev = null
          res
        } else
          throw new NoSuchElementException( "hub empty" )
      }
    }

  def basic = {
    val hub = new Hub( false )

    (hub, Source.fromIterator( () => iterator(hub) ))
  }

//  def keepAlive = {
//    val hub = new Hub( false )
//
//    (hub, Source.fromIterator( () =>
//      new Iterator[Double] {
//        val hasNext = true
//
//        def next = hub.pull.getOrElse( 0 )
//      } ) )
//  }

  def keepAlive = {
    val hub = new Hub( true )

    (hub, Source.fromIterator( () => iterator(hub) ))
  }

}

class Hub( keepAlive: Boolean) {

  private val inputs = new ArrayBuffer[SinkQueueWithCancel[Double]]
  private var cancelled = false

  def plug( src: Source[Double, _] ) = synchronized {
    inputs += src.runWith( Sink.queue[Double] )
  }

  def cancel = {
    cancelled = true
  }

  private def pull = synchronized {
    val buf = Await.result( Future.sequence( inputs map (_.pull) ), Duration.Inf )

    def clean: Unit = {
      buf indexOf None match {
        case -1 =>
        case idx =>
          buf remove idx
          inputs remove idx
          clean
      }
    }

    clean

    if (cancelled || (buf.isEmpty && !keepAlive))
      None
    else
      Some( buf map (_.get) sum )
  }

}