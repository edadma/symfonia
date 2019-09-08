package xyz.hyperreal

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.collection.immutable.ArraySeq


package object symfonia {

//  type Sample = ArraySeq[Double]

  implicit val system = ActorSystem( "Symfonia" )
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

}