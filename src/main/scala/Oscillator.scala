package xyz.hyperreal.symfonia

import akka.stream.scaladsl.Source


object Oscillator {

  def forWaveFunction( freq: Source[Double, _], func: Int => Double ) = {
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

  def sinWave( freq: Source[Double, _] ) = forWaveFunction( freq, Symfonia.sinWavetable )

  def pulseWave( freq: Source[Double, _], duty: Double ) = forWaveFunction( freq, n => if (n < Symfonia.sps*duty) 1 else -1 )

  def squareWave( freq: Source[Double, _] ) = pulseWave( freq, .5 )

  def sawWave( freq: Source[Double, _] ) = forWaveFunction( freq, n => n.toDouble/Symfonia.sps*2 - (if (n <= Symfonia.sps/2) 0 else 2) )

  def triangleWave( freq: Source[Double, _] ) = forWaveFunction( freq, n => if (n <= Symfonia.sps/4) n.toDouble/Symfonia.sps*4 else if (n <= Symfonia.sps*3/4) 1 - (n.toDouble/Symfonia.sps*4 - 1) else n.toDouble/Symfonia.sps*4 - 4 )

}
