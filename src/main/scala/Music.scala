package xyz.hyperreal.symfonia

import scala.collection.immutable.ArraySeq
import math._


object Music {

  val octave =
    ArraySeq(
      Seq( "C" ),
      Seq( "C#", "Db"),
      Seq( "D" ),
      Seq( "D#", "Eb" ),
      Seq( "E" ),
      Seq( "F" ),
      Seq( "F#", "Gb" ),
      Seq( "G" ),
      Seq( "G#", "Ab" ),
      Seq( "A" ),
      Seq( "A#", "Bb" ),
      Seq( "B" )
    )

  val notes =
    ArraySeq( (for (i <- 0 to 127) yield Note( i, octave(i%12), i/12 - 1, i%12, 440*pow(2, (i - 69)/12d), octave(i%12).size )): _* )

  val noteMap =
    notes flatMap (n => n.names map (name => ((name, n.octave) -> n)))
}

case class Note( n: Int, names: Seq[String], octave: Int, note: Int, freq: Double, typ: Int )