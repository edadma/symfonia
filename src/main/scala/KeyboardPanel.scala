package xyz.hyperreal.symfonia

import java.awt.Color._
import java.awt.geom.Path2D

import scala.swing.{Graphics2D, Panel}
import scala.swing.Swing._
import scala.swing.event.{MouseExited, MouseMoved, MousePressed, MouseReleased}


class KeyboardPanel( startNote: Int, endNote: Int, widthWhite: Int, heightWhite: Int, widthBlack: Int, heightBlack: Int, shift: Int, spacing: Int ) extends Panel {

  require( Music.notes(startNote).typ == 1 && Music.notes(endNote).typ == 1 )

  val nat = for (i <- startNote to endNote if Music.notes(i).typ == 1) yield widthWhite
  val width = nat.sum + (nat.length - 1)*spacing

  preferredSize = (width, heightWhite)
  background = BLACK
  foreground = WHITE

  def pathPlain( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x, y)
    path lineTo (x + widthWhite, y)
    path lineTo (x + widthWhite, y + heightWhite)
    path lineTo (x, y + heightWhite)
    path.closePath
    path
  }

  def pathBlack( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x, y)
    path lineTo (x + widthBlack, y)
    path lineTo (x + widthBlack, y + heightBlack)
    path lineTo (x, y + heightBlack)
    path.closePath
    path
  }

  def pathCF( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x, y)
    path lineTo (x + widthWhite - widthBlack/2 - shift - spacing, y)
    path lineTo (x + widthWhite - widthBlack/2 - shift - spacing, y + heightBlack + spacing)
    path lineTo (x + widthWhite, y + heightBlack + spacing)
    path lineTo (x + widthWhite, y + heightWhite)
    path lineTo (x, y + heightWhite)
    path.closePath
    path
  }

  def pathD( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x + widthBlack/2 - shift, y)
    path lineTo (x + widthWhite - widthBlack/2 + shift - spacing, y)
    path lineTo (x + widthWhite - widthBlack/2 + shift - spacing, y + heightBlack + spacing)
    path lineTo (x + widthWhite, y + heightBlack + spacing)
    path lineTo (x + widthWhite, y + heightWhite)
    path.lineTo (x, y + heightWhite)
    path lineTo (x, y + heightBlack + spacing)
    path lineTo (x + widthBlack/2 - shift, y + heightBlack + spacing)
    path.closePath
    path
  }

  def pathA( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x + widthBlack/2, y)
    path lineTo (x + widthWhite - widthBlack/2 + shift - spacing, y)
    path lineTo (x + widthWhite - widthBlack/2 + shift - spacing, y + heightBlack + spacing)
    path lineTo (x + widthWhite, y + heightBlack + spacing)
    path lineTo (x + widthWhite, y + heightWhite)
    path.lineTo (x, y + heightWhite)
    path lineTo (x, y + heightBlack + spacing)
    path lineTo (x + widthBlack/2, y + heightBlack + spacing)
    path.closePath
    path
  }

  def pathG( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x + widthBlack/2 - shift, y)
    path lineTo (x + widthWhite - widthBlack/2 - spacing, y)
    path lineTo (x + widthWhite - widthBlack/2 - spacing, y + heightBlack + spacing)
    path lineTo (x + widthWhite, y + heightBlack + spacing)
    path lineTo (x + widthWhite, y + heightWhite)
    path.lineTo (x, y + heightWhite)
    path lineTo (x, y + heightBlack + spacing)
    path lineTo (x + widthBlack/2 - shift, y + heightBlack + spacing)
    path.closePath
    path
  }

  def pathEB( x: Int, y: Int ) = {
    val path = new Path2D.Double

    path moveTo (x + widthBlack/2 + shift, y)
    path lineTo (x + widthWhite, y)
    path lineTo (x + widthWhite, y + heightWhite)
    path.lineTo (x, y + heightWhite)
    path lineTo (x, y + heightBlack + spacing)
    path lineTo (x + widthBlack/2 + shift, y + heightBlack + spacing)
    path.closePath
    path
  }

  val keyPaths = {
    var x = 0
    var prev: Note = null

    for (n <- startNote to endNote)
      yield {
        val v =
          Music.notes(n).names.head match {
            case "C" => pathCF( x, 0 )
            case "C#"|"F#" => pathBlack( x - spacing - widthBlack/2 - shift, 0 )
            case "A#"|"D#" => pathBlack( x - spacing - widthBlack/2 + shift, 0 )
            case "G#" => pathBlack( x - spacing - widthBlack/2, 0 )
            case "D" => pathD( x, 0 )
            case "E" => pathEB( x, 0 )
            case "F" => pathCF( x, 0 )
            case "G" => pathG( x, 0 )
            case "A" => pathA( x, 0 )
            case "B" => pathEB( x, 0 )
          }

        if (Music.notes(n).typ == 1)
          x += widthWhite + spacing

        v
      }
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
      else if (Music.notes(k + startNote).typ == 1)
        g setColor WHITE
      else
        g setColor DARK_GRAY

      g fill p
    }
  }

}