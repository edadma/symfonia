package xyz.hyperreal.symfonia

import akka.NotUsed
import akka.stream._
import akka.stream.scaladsl._

import math.{Pi, sin}
import scala.collection.immutable.ArraySeq

import java.nio.file.Path


object Symfonia {

  private [symfonia] var _sps = 12//44100

  def sps = _sps

  def sps_=( n: Int ): Unit = {
    require( 8000 <= n && n <= 96000, s"samples per second is out of range: $n" )

    _sps = n
    init
  }

  private def computeSinWavetable = ArraySeq( (for (i <- 0 until _sps) yield sin( 2*Pi/_sps*i )): _* )

  private [symfonia] var sinWavetable = computeSinWavetable

  def init: Unit = {
    sinWavetable = computeSinWavetable
  }

  def dup( src: Source[Double, NotUsed] ) = src map (s => (s, s))

}

object Mixer {

  def apply( srcs: Seq[Source[Double, NotUsed]] ) = {
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

          n = (n + f) % Symfonia._sps
          v
        }
      }
    } )
  }

  def sinWave( freq: Double ) = forWaveFunction( Source.repeat(freq), Symfonia.sinWavetable )

  def pulseWave( freq: Double, duty: Double ) = forWaveFunction( Source.repeat(freq), n => if (n < Symfonia._sps*duty) 1 else -1 )

  def squareWave( freq: Double ) = pulseWave( freq, .5 )

  def sawWave( freq: Double ) = forWaveFunction( Source.repeat(freq), n => n.toDouble/Symfonia._sps*2 - (if (n <= Symfonia._sps/2) 0 else 2) )

  def triangleWave( freq: Double ) = forWaveFunction( Source.repeat(freq), n => if (n <= Symfonia._sps/4) n.toDouble/Symfonia._sps*4 else if (n <= Symfonia._sps*3/4) 1 - (n.toDouble/Symfonia._sps*4 - 1) else n.toDouble/Symfonia._sps*4 - 4 )

  def sinWave( freq: Source[Double, NotUsed] ) = forWaveFunction( freq, Symfonia.sinWavetable )

  def pulseWave( freq: Source[Double, NotUsed], duty: Double ) = forWaveFunction( freq, n => if (n < Symfonia._sps*duty) 1 else -1 )

  def squareWave( freq: Source[Double, NotUsed] ) = pulseWave( freq, .5 )

  def sawWave( freq: Source[Double, NotUsed] ) = forWaveFunction( freq, n => n.toDouble/Symfonia._sps*2 - (if (n <= Symfonia._sps/2) 0 else 2) )

  def triangleWave( freq: Source[Double, NotUsed] ) = forWaveFunction( freq, n => if (n <= Symfonia._sps/4) n.toDouble/Symfonia._sps*4 else if (n <= Symfonia._sps*3/4) 1 - (n.toDouble/Symfonia._sps*4 - 1) else n.toDouble/Symfonia._sps*4 - 4 )

}
