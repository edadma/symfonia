package xyz.hyperreal.symfonia

import akka.NotUsed
import akka.stream._
import akka.stream.scaladsl._

import math.{Pi, sin}
import scala.collection.immutable.ArraySeq


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

  def envelope( src: Source[Double, _], func: Source[Double, _] ) = (src zipWith func)( _*_ )

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
