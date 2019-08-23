package xyz.hyperreal.symfonia

import java.io.ByteArrayInputStream
import java.nio.file.Path

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success

import akka.stream.scaladsl.{Sink, Source}
import javax.sound.sampled.{AudioFileFormat, AudioFormat, AudioInputStream, AudioSystem}


object Output {

  def toMonoPCMInts( src: Source[Double, _] ) = src map (s => (s*32767).toInt)

  def toMonoPCMBytes( src: Source[Double, _] ) =
    src mapConcat {
      s =>
        val a = (s*32767).toInt

        List( (a >> 8) toByte, a toByte )
    }

  def toMonoWaveFile( src: Source[Double, _], file: Path ): Unit = {
    val future = toMonoPCMBytes( src ).runWith( Sink.seq )

    future.onComplete {
      case Success( seq ) =>
        val array = seq.toArray
        val format = new AudioFormat( Symfonia._sps, 16, 1, true, true )
        val stream = new AudioInputStream( new ByteArrayInputStream(array), format, array.length )

        AudioSystem.write( stream, AudioFileFormat.Type.WAVE, file.toFile )
    }

    Await.ready( future, Duration.Inf )
  }

}