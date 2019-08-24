package xyz.hyperreal.symfonia

import scala.collection.immutable.ArraySeq
import math._


object Music {

  val octave =
    ArraySeq(
      Set( "C" ),
      Set( "C#", "Db"),
      Set( "D" ),
      Set( "D#", "Eb" ),
      Set( "E" ),
      Set( "F" ),
      Set( "F#", "Gb" ),
      Set( "G" ),
      Set( "G#", "Ab" ),
      Set( "A" ),
      Set( "A#", "Bb" ),
      Set( "B" )
    )

  val notes =
    ArraySeq( (for (i <- 0 to 127) yield Note( i, octave(i%12), i/12 - 1, i%12, 440*pow(2, (i - 69)/12d), octave(i%12).size )): _* )

  val noteMap =
    notes flatMap (n => n.names map (name => ((name, n.octave) -> n)))
}

case class Note( n: Int, names: Set[String], octave: Int, note: Int, freq: Double, typ: Int )