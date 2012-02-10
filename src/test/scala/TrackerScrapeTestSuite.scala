package test.scala

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll

import grizzled.slf4j.Logger

import scala.collection.mutable.ArrayBuffer
import com.submarinerich.storrent.{Torrent,Tracker,TorrentClient}
import com.submarinerich.storrent.handlers.{TrackerUDPHandlerPipelineFactory}
import org.jboss.netty.channel.socket.nio.NioDatagramChannelFactory
import org.jboss.netty.bootstrap.ClientBootstrap
import org.jboss.netty.channel.{ChannelFuture,ChannelFutureListener}
import org.jboss.netty.buffer.ChannelBuffer
import java.util.concurrent.Executors

class TrackerScrapeTestSuite extends FunSuite with ShouldMatchers {

	var log = Logger(classOf[TrackerScrapeTestSuite] )

  test("test to see if a tracker can scrape effectively"){
    var torrenturl = RandomTorrent.get
    var t = Torrent.fromURL(torrenturl)
    var ucf = new NioDatagramChannelFactory( Executors.newCachedThreadPool )
    var ub : ClientBootstrap = new ClientBootstrap(ucf)
    ub.setOption("broadcast",true) 
    ub.setOption("sendBufferSize", 65536)
    ub.setOption("receiveBufferSize", 65536)
    ub.setOption("keepAlive",true)
    ub.setOption("tcpNoDelay",true)
    for( tracker <- t.udptrackers ){
      ub.setPipelineFactory( new TrackerUDPHandlerPipelineFactory(t,tracker) )
      var channelFuture = ub.connect(tracker.socketAddress)
      channelFuture.addListener( new ChannelFutureListener(){
        def operationComplete( future : ChannelFuture ){
          var scrapeRequest : Tuple2[ChannelBuffer,Int] = tracker.scrapeRequestBuffer(t)
          if( future.isSuccess )
            future.getChannel.write(scrapeRequest._1)
        }
      })
    }
    Thread.sleep(3000L)
    var responses = 0
    for( tracker <- t.udptrackers ){
      responses += tracker.scrapeResponses.size
    }
    responses should be >= (1)
  }
}