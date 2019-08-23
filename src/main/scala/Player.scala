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

//  def apply( src: Source[Double, _] ): PlayerControl = {
//
//    new PlayerControl {
//      var playing = false
//
//      object playlock
//
//      val thread =
//        new Thread {
//          override def run = {
//            val blocks = Output.toMonoPCMBytes( src ) grouped line.getBufferSize
//
//            open
//            line.start
//
//            blocks runForeach {
//              block =>
//                val array = block.toArray
//
//                while (!playing)
//                  Thread.`yield`
//
//                line.write( array, 0, array.length )
//            }
//
//            line.drain
//            line.stop
//          }
//        }
//
//      thread.start
//
//      def play: PlayerControl = {
//        playing = true
//        this
//      }
//
//      def pause = playing = false
//
//      def join = thread.join
//    }
//
//  }

    def apply( src: Source[Double, _] ): PlayerControl = {

      new PlayerControl {
        abstract class PlayerState
        case object INITIAL extends PlayerState
        case object PLAYING extends PlayerState
        case object PAUSING extends PlayerState
        case object STOPPED extends PlayerState

        var state: PlayerState = INITIAL

        val thread =
          new Thread {
            override def run = {
              val sink = Output.toMonoPCMBytes( src ) grouped line.getBufferSize runWith Sink.queue[Seq[Byte]]

              open
              line.start

              def pull: Unit = {
                while (state == PAUSING)
                  Thread.`yield`

                if (state == PLAYING) {
                  Await.result( sink.pull, Duration.Inf ) match {
                    case None =>
                      line.drain
                      line.stop
                    case Some( block ) =>
                      val array = block.toArray

                      line.write( array, 0, array.length )
                      pull
                  }
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

  def join: Unit

}