package xyz.hyperreal.symfonia

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.file.Path

import akka.NotUsed
import akka.stream.scaladsl.{Sink, Source, StreamConverters}
import akka.util.ByteString
import javax.sound.sampled.{AudioFileFormat, AudioFormat, AudioInputStream}

import scala.util.Success


object Output {

  def toMonoPCMInts( src: Source[Double, NotUsed] ) = src map (s => (s*32767).toInt)

  def toMonoPCMBytes( src: Source[Double, NotUsed] ) =
    src mapConcat {
      s =>
        val a = (s*32767).toInt

        List( (a >> 8) toByte, a toByte )
    }

  def toMonoFile( src: Source[Double, NotUsed], file: Path ) = {
    val future = toMonoPCMBytes( src ).runWith( Sink.seq ) // StreamConverters.asInputStream()

    future.onComplete {
      case Success( seq ) =>
        val array = seq.toArray
        val format = new AudioFormat( Symfonia._sps, 16, 1, true, true )
        val stream = new AudioInputStream( new ByteArrayInputStream(array), format, array.length )
    }

//    AudioFileFormat.Type.WAVE,
  }

}