package xyz.hyperreal.symfonia

import java.awt.{BasicStroke, Color}
import java.awt.geom.Path2D
import java.awt.RenderingHints._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success
import akka.NotUsed
import akka.stream.scaladsl.{Sink, Source}

import scala.swing.{Graphics2D, MainFrame, Panel}
import scala.swing.Swing._


object Scope {

  def apply( src: Source[Double, _] ): Unit = {
    val future = src.take( 500 ) runWith Sink.seq

    future.onComplete {
      case Success( seq ) =>
        new MainFrame {
          contents =
            new Panel {
              preferredSize = (500, 201)
              background = Color.BLACK

              val points = seq map (-_*100 + 100)
              val path =
                new Path2D.Double {
                  moveTo( 0, points.head )

                  for ((y, n) <- points.tail.zipWithIndex)
                    lineTo( n + 1, y )
                }

              override def paintComponent( g: Graphics2D ): Unit = {
                super.paintComponent( g )

                g.setColor( Color.GREEN )
                g.setStroke( new BasicStroke(3) )
                g.setRenderingHint( KEY_ANTIALIASING, VALUE_ANTIALIAS_ON )
                g.draw( path )
              }
            }

          centerOnScreen
          pack
          resizable = false
          open
        }
    }

    Await.ready( future, 5.second )
  }

}