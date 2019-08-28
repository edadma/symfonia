package xyz.hyperreal.symfonia

import java.nio.FloatBuffer
import java.util.ServiceLoader
import java.util.logging.Level
import java.util.logging.Logger

import scala.jdk.CollectionConverters._

import akka.stream.scaladsl.{Sink, Source}

import org.jaudiolibs.audioservers.AudioClient
import org.jaudiolibs.audioservers.AudioConfiguration
import org.jaudiolibs.audioservers.AudioServerProvider
import org.jaudiolibs.audioservers.ext.ClientID
import org.jaudiolibs.audioservers.ext.Connections



object Player {

  class PlayerClient extends AudioClient {
    val loader = ServiceLoader.load(classOf[AudioServerProvider])

    def findProvider: Option[AudioServerProvider] = {
      for (p <- loader.iterator.asScala) {
        if (p.getLibraryName == "JACK")
          return Some( p )
      }

      None
    }

    val provider =
      findProvider match {
        case Some( p ) => p
        case None => sys.error( "jack not found" )
      }

    val config = new AudioConfiguration(
      44100.0f, //sample rate
      0, // input channels
      2, // output channels
      256, //buffer size
      // extensions
      new ClientID("Symfonia"),
      Connections.OUTPUT)


    /* Use the AudioServerProvider to create an AudioServer for the client.
     */
    val server = provider.createServer(config, this)

    def configure( context: AudioConfiguration ): Unit = {

    }

    def process( time: Long, inputs: java.util.List[FloatBuffer], outputs: java.util.List[FloatBuffer], nframes: Int ) = {

    }

    def shutdown = {

    }
  }

}