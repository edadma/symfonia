package xyz.hyperreal.symfonia

import java.nio.file.Path

import akka.NotUsed
import akka.stream.scaladsl.{Source, StreamConverters}


object Output {

  def toMonoPCMInts( src: Source[Double, NotUsed] ) = src map (s => (s*32767).toInt)

  def toMonoPCMBytes( src: Source[Double, NotUsed] ) =
    src mapConcat {
      s =>
        val a = (s*32767).toInt

        List( (a >> 8) toByte, a toByte )
    }

  def toMonoFile( src: Source[Double, NotUsed], file: Path ) = {
    toMonoPCMBytes( src ).runWith( StreamConverters.asInputStream() )
  }

}