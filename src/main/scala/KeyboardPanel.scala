package xyz.hyperreal.symfonia

import java.awt.Color._
import java.awt.geom.Path2D

import scala.swing.{Graphics2D, MainFrame, Panel}
import scala.swing.Swing._
import scala.swing.event.{MouseEntered, MouseExited, MouseMoved, MousePressed, MouseReleased}


class KeyboardPanel( startNote: Int, endNote: Int, widthWhite: Int, heightWhite: Int, widthBlack: Int, heightBlack: Int, shift: Int, spacing: Int ) extends Panel {

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
  background = BLACK
  foreground = WHITE

  def pathPlain( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x, y)
    path lineTo (x + widthWhite, y)
    path lineTo (x + widthWhite, y + heightWhite)
    path.lineTo (x, y + heightWhite)
    path lineTo (x, y)
    path
  }

  def pathBlack( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x, y)
    path lineTo (x + widthBlack, y)
    path lineTo (x + widthBlack, y + heightBlack)
    path.lineTo (x, y + heightBlack)
    path lineTo (x, y)
    path
  }

  def pathCE( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x, y)
    path lineTo (x + widthWhite - widthBlack/2 - shift - spacing, y)
    path lineTo (x + widthWhite - widthBlack/2 - shift - spacing, y + heightBlack)
    path lineTo (x + widthWhite, y + heightBlack)
    path lineTo (x + widthWhite, y + heightWhite)
    path.lineTo (x, y + heightWhite)
    path lineTo (x, y)
    path
  }

  val keyPaths = {
    Seq( pathCE( 0, 0 ) )
//    var x = 0
//    var prev: Note = _
//
//    for (n <- startNote to endNote)
//      yield {
//
//      }
  }

  var keyhover = false
  var keypress = false
  var key: Int = _

  listenTo( mouse.clicks, mouse.moves )

  reactions += {
    case MouseExited( _, _, _) =>
      if (keyhover || keypress) {
        keyhover = false
        keypress = false
        repaint
      }
    case MouseMoved( _, p, _ ) =>
      var within = false

      for (k <- keyPaths.indices)
        if (keyPaths(k).contains( p )) {
          if (!keypress && (!keyhover || key != k)) {
            keyhover = true

            key = k
            repaint
          }

          within = true
        }

      if (!within && (keyhover || keypress)) {
        repaint
        keyhover = false
        keypress = false
      }
    case MousePressed( _, p, _, 1, _ ) =>
      var within = false

      for (k <- keyPaths.indices)
        if (keyPaths(k).contains( p )) {
          if (!keypress || key != k) {
            keyhover = false
            keypress = true
            key = k
            repaint
          }

          within = true
        }

      if (!within && (keyhover || keypress)) {
        repaint
        keyhover = false
        keypress = false
      }
    case MouseReleased( _, p, _, _, _ ) =>
      var within = false

      for (k <- keyPaths.indices)
        if (keyPaths(k).contains( p )) {
          keyhover = true
          keypress = false
          key = k
          within = true
          repaint
        }

      if (!within) {
        repaint
        keyhover = false
        keypress = false
      }
  }

  override def paintComponent( g: Graphics2D ): Unit = {
    super.paintComponent( g )

    for ((p, k) <- keyPaths.zipWithIndex) {
      if (keyhover && key == k)
        g setColor CYAN
      else if (keypress && key == k)
        g setColor GREEN
      else
       g setColor WHITE

      g fill p
    }
  }

}