package xyz.hyperreal.symfonia

import akka.stream.scaladsl.{Sink, Source}

import javax.sound.sampled.{AudioFormat, AudioSystem}
import java.util.concurrent.locks.ReentrantLock

import scala.concurrent.Await


object Player {

  var format: AudioFormat = _
  var line = AudioSystem.getSourceDataLine( format )
  var opened = false

  def open = {
    opened = true
    init
  }

  def close = {
    line.close
    opened = false
  }

  def init = {

    if (opened) {
      format = new AudioFormat( Symfonia.sps, 16, 1, true, true )
      line = AudioSystem.getSourceDataLine( format )
      line.open( format )
    }

  }

  def apply( src: Source[Double, _] ): PlayerControl = {

    new PlayerControl {
      var playing = false

      object playlock

      val thread =
        new Thread {
          override def run = {
            val blocks = Output.toMonoPCMBytes( src ) grouped line.getBufferSize

            open
            line.start

            blocks runForeach {
              block =>
                val array = block.toArray

                if (!playing)
                  playlock.wait

                line.write( array, 0, array.length )
            }

            line.drain
            line.stop
            close
          }
        }

      thread.start

      def play: PlayerControl = {
        if (!playing) {
          playing = true
          playlock.notify
        }

        this
      }

      def pause = playing = false

      def join = thread.join
    }

  }

}

trait PlayerControl {

  def play: PlayerControl

  def join: Unit

}