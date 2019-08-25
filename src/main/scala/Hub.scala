package xyz.hyperreal.symfonia

import akka.stream.scaladsl.{Sink, SinkQueueWithCancel, Source}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object Hub {

  def apply( src: Source[Double, _] ) = {
    val hub = new Hub

    (hub, Source.fromIterator( () =>
      new Iterator[Double] {
        val hasNext = true

        def next = hub.pull
      } ) )
  }

}

class Hub {

  private val inputs = new ArrayBuffer[SinkQueueWithCancel[Double]]

  def plug( src: Source[Double, _] ) = {
    inputs += src.runWith( Sink.queue[Double] )
  }

  private def pull = {
    val buf = Await.result( Future.sequence( inputs map (_.pull) ), Duration.Inf )

    def clean: Unit = {
      buf indexOf None match {
        case -1 =>
        case idx =>
          buf remove idx
          inputs remove idx
          clean
      }
    }

    clean
    buf map (_.get) sum
  }

}