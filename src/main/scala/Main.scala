package xyz.hyperreal.symfonia

import java.nio.file.Paths

import akka.actor.ActorSystem
import akka.actor.Status.Success
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}

import scala.swing.MainFrame


object Main extends App {

  val fund = 220
  val h1 = Oscillator.sinWave( fund )
  val h2 = Shape.amplitude( Oscillator.sinWave(fund*3), 1/3d )
  val h3 = Shape.amplitude( Oscillator.sinWave(fund*5), 1/5d )
  val h4 = Shape.amplitude( Oscillator.sinWave(fund*7), 1/7d )
  val h5 = Shape.amplitude( Oscillator.sinWave(fund*9), 1/9d )
  val src = Shape.length( h1, 1 ) concat
    Shape.length( Mixer( List(h1, h2) ), 1 ) concat
    Shape.length( Mixer( List(h1, h2, h3) ), 1 ) concat
    Shape.length( Mixer( List(h1, h2, h3, h4) ), 1 ) concat
    Shape.length( Mixer( List(h1, h2, h3, h4, h5) ), 1 )

//  Player( Shape.length(Oscillator.sinWave(440), 1) ).play.join

//  Output.toMonoWaveFile( src, Paths.get("tone.wav") )
//  Scope( src )

  new MainFrame {
    contents = new KeyboardPanel( 72, 83, 40, 120, 20, 80, 5, 2 )
    centerOnScreen
    pack
    resizable = false
    open
  }

  system.terminate

}