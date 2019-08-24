package xyz.hyperreal.symfonia

import akka.stream.scaladsl.{Sink, Source}
import javax.sound.sampled.{AudioFormat, AudioSystem}
import java.util.concurrent.locks.ReentrantLock

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Success


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
      if (format != null && format.getFrameRate.toInt != Symfonia.rate) {
        line.close
        format = null
      }

      if (format eq null) {
        format = new AudioFormat( Symfonia.rate, 16, 1, true, true )
        line = AudioSystem.getSourceDataLine( format )
        line.open( format )
      }
    }

  }

  def apply( src: Source[Double, _] ): PlayerControl = {

    new PlayerControl {
      abstract class PlayerState
      case object INITIAL extends PlayerState
      case object PLAYING extends PlayerState
      case object PAUSING extends PlayerState
      case object STOPPED extends PlayerState

      var _state: PlayerState = INITIAL

      object statesync

      def state = statesync synchronized( _state )

      def state_=( newstate: PlayerState ) = statesync synchronized {_state = newstate }

      val thread =
        new Thread {
          override def run = {
            val sink = Output.toMonoPCMBytes( src ) grouped line.getBufferSize runWith Sink.queue[Seq[Byte]]

            open
            line.start

            def pull: Unit = {
              state match {
                case PAUSING =>
                  Thread.`yield`
                  pull
                case PLAYING =>
                  Await.result( sink.pull, Duration.Inf ) match {
                    case None =>
                      line.drain
                      line.stop
                    case Some( block ) =>
                      val array = block.toArray

                      line.write( array, 0, array.length )
                      pull
                  }
                case STOPPED =>
                  sink.cancel
                  line.stop
              }
            }

            while (state == INITIAL)
              Thread.`yield`

            pull
          }
        }

      thread.start

      def play: PlayerControl = {
        state = PLAYING
        this
      }

      def pause = state = PAUSING

      def stop = state = STOPPED

      def join = thread.join
    }

  }

}

trait PlayerControl {

  def play: PlayerControl

  def pause: Unit

  def stop: Unit

  def join: Unit

}