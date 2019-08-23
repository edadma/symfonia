package xyz.hyperreal.symfonia

import akka.stream.scaladsl.{Sink, Source}
import javax.sound.sampled.{AudioFormat, AudioSystem}

import scala.concurrent.Await


object Player {

  def apply( src: Source[Double, _] ) = {

    val thread =
      new Thread {
        override def run = {
          val format = new AudioFormat( Symfonia.sps, 16, 1, true, true )
          val line = AudioSystem.getSourceDataLine( format )
          val blocks = Output.toMonoPCMBytes( src ) grouped line.getBufferSize

          line.open( format )
          line.start

          blocks runForeach {
            block =>
              val array = block.toArray

              line.write( array, 0, array.length )
          }

          line.drain
          line.stop
          line.close
        }
      }

    thread.start
    thread.join

  }

}

trait PlayerControl {



}