package xyz.hyperreal.symfonia

import akka.actor.ActorSystem
import akka.actor.Status.Success
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink


object Main extends App {

  implicit val system = ActorSystem( "Symfonia" )
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val source = Oscillator.sinWave( 1 ).take( 5 )
  val result = source.runWith( Sink.seq )

  result.onComplete {
    case scala.util.Success( s ) =>
      println( s map ("%.2f" format _) )
      system.terminate
  }

}