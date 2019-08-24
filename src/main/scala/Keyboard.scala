package xyz.hyperreal.symfonia

import java.awt.Color

import scala.swing.{Graphics2D, MainFrame, Panel}
import scala.swing.Swing._


class Keyboard( startNote: Int, endNote: Int, widthWhite: Int, heightWhite: Int, widthBlack: Int, heightBlack: Int, shift: Int, spacing: Int ) extends Panel {

  require( Music.notes(startNote).typ == 1 && Music.notes(endNote).typ == 1 )

  val width = {
    (for (i <- startNote to endNote)
      yield {
        val note = Music.notes(i)

        if (note.typ == 1)
          widthWhite
        else
          widthBlack
      }).sum + (endNote - startNote)*spacing}

  preferredSize = (width, heightWhite)
  background = Color.BLACK
  foreground = Color.WHITE



}