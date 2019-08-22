package xyz.hyperreal.symfonia

import akka.stream._
import akka.stream.scaladsl._

import math.{Pi, sin}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer


object Symfonia {

  private [symfonia] var _sps = 44100

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

}

object Oscillator {

  def forWaveFunction(freq: Double, func: Int => Double ) = {
    Source.fromIterator(
      () =>
        new Iterator[Double] {
          var n = 0.0

          val hasNext = true

          def next = {
            val v = func( n.toInt )

            n = (n + freq) % Symfonia._sps
            v
          }
        }
    )
  }

  def sinWave( freq: Double ) = forWaveFunction( freq, Symfonia.sinWavetable )

  def pulseWave( freq: Double, duty: Double = .05 ) = forWaveFunction( freq, n => if (n < Symfonia._sps*duty) 1 else -1 )

  def squareWave( freq: Double ) = pulseWave( freq, .5 )

  def sawWave( freq: Double ) = forWaveFunction( freq, n => n.toDouble/Symfonia._sps*2 - (if (n <= Symfonia._sps/2) 0 else 2) )

  def triangleWave( freq: Double ) = forWaveFunction( freq, n => if (n <= Symfonia._sps/4) n.toDouble/Symfonia._sps*4 else if (n <= Symfonia._sps*3/4) 1 - (n.toDouble/Symfonia._sps*4 - 1) else n.toDouble/Symfonia._sps*4 - 4 )

}
