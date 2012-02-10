package com.submarinerich.storrent.handlers

import org.jboss.netty.channel.{ChannelPipelineFactory,Channels,ChannelPipeline,ChannelState,WriteCompletionEvent,ChannelStateEvent}
import org.jboss.netty.channel.{MessageEvent,ChannelHandlerContext,ChannelEvent,Channel}
import org.jboss.netty.handler.codec.replay.{ReplayingDecoder,VoidEnum}
import org.jboss.netty.buffer.{ChannelBuffer,ChannelBuffers}
import com.twitter.conversions.time._
import com.twitter.util.{Time,Duration}
import com.submarinerich.storrent.{Torrent,Tracker,ScrapeResponse,Peer,TorrentClient}
import com.submarinerich.storrent.actors._
import com.submarinerich.storrent.util.ByteUtils
import java.nio.ByteBuffer
import scala.util.Random
import grizzled.slf4j.Logger

class TrackerUDPHandlerPipelineFactory( t: Torrent, tr: Tracker ) extends ChannelPipelineFactory 
{
  var torrent = t
  var tracker = tr
  var log = Logger(classOf[TrackerUDPHandlerPipelineFactory] )
  def getPipeline = {
    var c = Channels.pipeline()
    c.addLast("handler",new TrackerUDPHandler(torrent,tracker) )
    c
  }
}

class TrackerUDPHandler(  t: Torrent, tr : Tracker ) extends ReplayingDecoder[VoidEnum] 
{
  import org.jboss.netty.channel.ExceptionEvent

  private var random = new scala.util.Random( Time.now )
  var log = Logger(classOf[TrackerUDPHandler])
  var torrent = t
  var tracker = tr
  var requestMade : Time = Time.now

  override protected def decode(ctx: ChannelHandlerContext, channel: Channel,buf: ChannelBuffer,state : VoidEnum) : Peer = {

  	 var messageType = buf.readInt
//  	 log.info("asked to Decode! message type: "+messageType)
  	 if( messageType == 1 ){ //announce response
//  	 	log.info("it's announce!")
//     Announce Response: 

//         0           32-bit integer  action          1 // announce
//         4           32-bit integer  transaction_id
//         8           32-bit integer  interval
//         12          32-bit integer  leechers
//         16          32-bit integer  seeders
//         20 + 6 * n  32-bit integer  IP address
//         24 + 6 * n  16-bit integer  TCP port
//         20 + 6 * N
				var transaction_id = buf.readInt
				var interval = buf.readInt
				var leechers = buf.readInt
				var seeders = buf.readInt

				while( buf.readableBytes >= 6 ){
					var ipAddress = new Array[Byte](4)
					var port = new Array[Byte](2)

					for(i <- 0 until 4)
						ipAddress(i) = buf.readByte

					for(i <- 0 until 2 )
						port(i) = buf.readByte

					PeerWrangler ! new Peer( ipAddress, port, torrent, TorrentClient.peer_id )
				}
  	 }
  	 if( messageType == 2 ){ //scrape response

//         scrape response:
//         Offset      Size            Name            Value
//         0           32-bit integer  action          2 // scrape
//         4           32-bit integer  transaction_id
//         8 + 12 * n  32-bit integer  seeders
//         12 + 12 * n 32-bit integer  completed
//         16 + 12 * n 32-bit integer  leechers
//         8 + 12 * N

				var transaction_id = buf.readInt
				var seeders = buf.readInt
				var completed = buf.readInt
				var leechers = buf.readInt
				tracker.scrapeResponses += new ScrapeResponse( torrent.info_hash_hex, seeders,completed,leechers)
  	 }
  	 null
  }

  override def writeComplete( ctx : ChannelHandlerContext, e : WriteCompletionEvent ) : Unit  = {
    super.writeComplete(ctx,e)
  }

  override def channelConnected( ctx : ChannelHandlerContext, e : ChannelStateEvent ) : Unit = {
    //log.info("connected! "+this)
    super.channelConnected(ctx,e)
  }

  override def messageReceived( ctx : ChannelHandlerContext, e : MessageEvent ) : Unit = {
    super.messageReceived(ctx,e)
  }

  override def exceptionCaught( ctx : ChannelHandlerContext, e : ExceptionEvent ) : Unit = {
    e.getChannel.close
    super.exceptionCaught(ctx,e)
  }

  override def channelDisconnected( ctx : ChannelHandlerContext, e : ChannelStateEvent ) : Unit = {
    //log.info("channel disconnected "+this)
    super.channelDisconnected(ctx,e)
  }
}