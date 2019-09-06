package xyz.hyperreal.symfonia

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.texish.{Active, Command, Parser, Renderer}


object Score {

  class Builder extends Renderer( parser, config, a => a ) {

  }

  val chordBegin =
    new Active( "<" ) {
      def apply( pos: Reader, r: Renderer ) = {

      }
    }
  val active = List( chordBegin )
  val parser = new Parser( Command.standard, active )
  val config =
    Map(
      "today" -> "MMMM d, y",
      "include" -> ".",
      "rounding" -> "HALF_EVEN"
    )

}

/*

\title <title string>
\subtitle <title string>

\written <date>

\author <author string>

\time <integer> <integer>

\key <key signature string>

\tempo 4=96

 */