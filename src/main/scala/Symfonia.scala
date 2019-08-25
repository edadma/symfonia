package xyz.hyperreal.symfonia

import akka.NotUsed
import akka.stream._
import akka.stream.scaladsl._

import math.{Pi, sin}
import scala.collection.immutable.ArraySeq

import java.nio.file.Path


object Symfonia {

  private [symfonia] var sps = 44100

  def rate = sps

  def rate_=( n: Int ): Unit = {
    require( 8000 <= n && n <= 96000, s"samples per second is out of range: $n" )

    sps = n
    init
  }

  private def computeSinWavetable = ArraySeq( (for (i <- 0 until sps) yield sin( 2*Pi/sps*i )): _* )

  private [symfonia] var sinWavetable = computeSinWavetable

  def init: Unit = {
    sinWavetable = computeSinWavetable
  }

  def dup( src: Source[Double, NotUsed] ) = src map (s => (s, s))

  def function( f: Double => Double ) =
    Source.fromIterator( () => new Iterator[Double] {
      var t = 0d

      val hasNext = true

      def next = {
        val v = f( t )

        t += 1d/Symfonia.rate
        v
      }
    } )

}

object Sound {

  def beep( freq: Double ) = Shape.length( Oscillator.sinWave(freq), .5 )

}

object Shape {

  def length( src: Source[Double, _], sec: Double ) = src take (sec*Symfonia.rate).toLong

  def amplitude( src: Source[Double, _], amplitude: Double ) = src map (_*amplitude)

  def fade( src: Source[Double, _], sec: Double ) = src take (sec*Symfonia.rate).toLong

}

object Mixer {

  def apply( srcs: Seq[Source[Double, _]] ) = {
    val len = srcs.length

    def mix( seq: Seq[Double] ) = seq sum

    Source.zipWithN( mix )( srcs )
  }

  def dampen( srcs: Seq[Source[Double, _]] ) = {
    val len = srcs.length

    def mix( seq: Seq[Double] ) = seq map (_/len) sum

    Source.zipWithN( mix )( srcs )
  }

}

object Oscillator {

  def forWaveFunction( freq: Source[Double, NotUsed], func: Int => Double ) = {
    Source.lazily( () => freq map {
      new Function[Double, Double] {
        var n = 0.0

        def apply( f: Double ) = {
          val v = func( n.toInt )

          n = (n + f) % Symfonia.sps
          v
        }
      }
    } )
  }

  def sinWave( freq: Double ) = forWaveFunction( Source.repeat(freq), Symfonia.sinWavetable )

  def pulseWave( freq: Double, duty: Double ) = forWaveFunction( Source.repeat(freq), n => if (n < Symfonia.sps*duty) 1 else -1 )

  def squareWave( freq: Double ) = pulseWave( freq, .5 )

  def sawWave( freq: Double ) = forWaveFunction( Source.repeat(freq), n => n.toDouble/Symfonia.sps*2 - (if (n <= Symfonia.sps/2) 0 else 2) )

  def triangleWave( freq: Double ) = forWaveFunction( Source.repeat(freq), n => if (n <= Symfonia.sps/4) n.toDouble/Symfonia.sps*4 else if (n <= Symfonia.sps*3/4) 1 - (n.toDouble/Symfonia.sps*4 - 1) else n.toDouble/Symfonia.sps*4 - 4 )

  def sinWave( freq: Source[Double, NotUsed] ) = forWaveFunction( freq, Symfonia.sinWavetable )

  def pulseWave( freq: Source[Double, NotUsed], duty: Double ) = forWaveFunction( freq, n => if (n < Symfonia.sps*duty) 1 else -1 )

  def squareWave( freq: Source[Double, NotUsed] ) = pulseWave( freq, .5 )

  def sawWave( freq: Source[Double, NotUsed] ) = forWaveFunction( freq, n => n.toDouble/Symfonia.sps*2 - (if (n <= Symfonia.sps/2) 0 else 2) )

  def triangleWave( freq: Source[Double, NotUsed] ) = forWaveFunction( freq, n => if (n <= Symfonia.sps/4) n.toDouble/Symfonia.sps*4 else if (n <= Symfonia.sps*3/4) 1 - (n.toDouble/Symfonia.sps*4 - 1) else n.toDouble/Symfonia.sps*4 - 4 )

}
