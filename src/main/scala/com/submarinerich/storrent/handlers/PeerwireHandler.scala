package com.submarinerich.storrent.handlers

import org.jboss.netty.channel.{ChannelState,WriteCompletionEvent,ChannelStateEvent,MessageEvent,ChannelHandlerContext}
import org.jboss.netty.channel.{ChannelEvent,SimpleChannelHandler,Channel,ChannelPipelineFactory,Channels,ChannelPipeline}
import org.jboss.netty.handler.codec.replay.{ReplayingDecoder,VoidEnum}
import org.jboss.netty.buffer.{ChannelBuffer,ChannelBuffers}
import com.twitter.conversions.time._
import com.twitter.util.{Time,Duration}
import com.submarinerich.storrent.{Peer,Torrent,Handshake,Message,MessageType,PieceOfPiece,TorrentPiece}
import com.submarinerich.storrent.actors.{Catalogue,PeerQuery,WaitingRoom,AddPatient,RemovePatient,Librarian,Input}
import com.submarinerich.storrent.util.ByteUtils
import scala.util.Random
import java.nio.ByteBuffer
import grizzled.slf4j.Logger

class PeerwireHandlerPipelineFactory( p : Peer, t: Torrent ) extends ChannelPipelineFactory 
{
  var torrent = t
  var peer = p
  var log = Logger(classOf[PeerwireHandlerPipelineFactory] )

  def getPipeline = {
    var c = Channels.pipeline()
    c.addLast("handler",new PeerwireHandler(peer,torrent) )
    c
  }
}


object PeerDecodingState extends Enumeration {
  type PeerDecodingState = Value
  val Unknown,JustMet,Handshaked,Interested,Choking = Value 
}

class PeerwireHandler( p : Peer, t: Torrent ) extends ReplayingDecoder[VoidEnum] 
{
  import com.submarinerich.storrent.AmState._
  import com.submarinerich.storrent.PeerState._
  import org.jboss.netty.channel.ExceptionEvent
  import PeerDecodingState._

  private val BASE_HANDSHAKE_LENGTH : Int = 49
  private val PIPELINE_SIZE : Int = 5
  private var random = new Random( Time.now )

  var log = Logger(classOf[PeerwireHandler])
  var peer = p
  var torrent = t
  var currentPipeline = 0
  var requestMade : Time = Time.now
  var amState : AmState = AmChoking
  var peerState : PeerState = PeerChoking
  var currentState : PeerDecodingState = JustMet

  override protected def decode(ctx: ChannelHandlerContext, channel: Channel,buf: ChannelBuffer,state : VoidEnum) : Message = {

     if( currentState == JustMet ){
      var pstrlen = buf.readByte()
      var handshake = buf.readBytes((pstrlen+BASE_HANDSHAKE_LENGTH)-1)
      var data = new Array[Byte]( pstrlen+BASE_HANDSHAKE_LENGTH )
      data(0) = pstrlen

      for( a <- 1 until data.length )
        data(a) = handshake.getByte(a-1)

      var hshake = Handshake.parse(data)
      p.peerId = Some( hshake.peer_id )
      //log.info("successfully handshake!")
      currentState = Handshaked
      channel.write( Message.interested )
      amState = AmInterested
      new Message( Array(0x00,0x00,0x00,0x00) )
     }else{
        var aml : Int = buf.readInt()
        if( aml == 0 )
         new Message( Array(0x00,0x00,0x00,0x00) )
        else{
        var actualMessage = buf.readBytes(aml)
        var amlBytes = ByteUtils.toByteArray(aml)
        var fullMessage = new Array[Byte]( 4 + aml )
        for(a <- 0 until 4)
          fullMessage(a) = amlBytes(a)
        for(a <- 0 until aml)
          fullMessage(4+a) = actualMessage.getByte(a)
        var m = new Message(fullMessage)
        // HAVE
        m.pieceIndex match {
          case Some(i : Int) => {
            Catalogue ! Input( torrent.info_hash_hex, peer, i )
          }
          case _ => {}
        }
        // BITFIELD
        m.pieces.map( a => {
            Catalogue ! Input( torrent.info_hash_hex, peer, a )
          })
        // UNCHOKE
        if( m.msgType == MessageType.Unchoke ){
          peerState = PeerInterested
          currentState = Interested
          currentPipeline = 0
        }
        // CHOKE
        if( m.msgType == MessageType.Choke ){
          peerState = PeerChoking
          currentState = Choking
          currentPipeline = 0
        }
        // PIECE
        m.dataChunk match {
          case Some( data : Tuple3[ByteBuffer,Int,Int]) => {
          	  //log.info("successful match "+data._1.array.length)
              Librarian ! new PieceOfPiece( torrent.info_hash_hex, data._2, data._3, data._1.array.size, Some(data._1) )
              currentPipeline -= 1
          }
          case _ => {}
        }

        if( m.msgType == MessageType.Interested ){
          channel.write( Message.bitfield(torrent) )
        }

        if( m.msgType == MessageType.NotInterested ){
          
        }

        if( m.msgType == MessageType.Request ){
          m.requestParams match {
            case Some( request : Tuple3[Int,Int,Int] ) => {
              //index, begin, block
              if( torrent.pieces(request._1).confirmed ){
                log.info("request I could be responding to!")
              }
            }
            case _ => {}
          }
        }

        if( currentPipeline == PIPELINE_SIZE && (Time.now - requestMade) > 30.seconds )
          currentPipeline = 0
        
        if( currentState == Interested && currentPipeline < PIPELINE_SIZE ){
          import scala.collection.mutable.ListBuffer
          var requestsToSend = PIPELINE_SIZE - currentPipeline
          var torrentPiecesToRequestFromThisPeer = new ListBuffer[TorrentPiece]()

          Catalogue !? PeerQuery( torrent.info_hash_hex, peer ) match {
            case l : List[Int] => torrent.pieces.map( a => if( l.contains(a.pieceIndex) && torrent.pieces(a.pieceIndex).confirmed == false ) torrentPiecesToRequestFromThisPeer += a )
            case _ => {}
          }

          if( torrentPiecesToRequestFromThisPeer.size > 0 ){
            import java.lang.Math.abs
            var lb = new ListBuffer[PieceOfPiece]()
            var dry = false
            while( requestsToSend > 0 && !dry ){
              var askFor = (abs(random.nextInt).toInt % torrentPiecesToRequestFromThisPeer.size) % 100 // ask for one of the N most rare pieces
              var pop = torrentPiecesToRequestFromThisPeer( askFor ).randomOutstandingPiece
              pop match {
                case Some( pop1 : PieceOfPiece ) => {
                  lb += pop1
                  requestsToSend -= 1
                }
                case _ => dry = true
              }
            }
            if( lb.size > 0 ){
              channel.write( Message.requestMultiple(lb.toList) )
              requestMade = Time.now
            }
          }
        }
        m
      }
    }
  }

  override def writeComplete( ctx : ChannelHandlerContext, e : WriteCompletionEvent ) : Unit  = {
    //log.info("write complete!  "+this)
    super.writeComplete(ctx,e)
  }

  override def channelConnected( ctx : ChannelHandlerContext, e : ChannelStateEvent ) : Unit = {
    //log.info("connected! "+this)
    WaitingRoom ! AddPatient( ctx.getChannel,peer )
    super.channelConnected(ctx,e)
  }

  override def messageReceived( ctx : ChannelHandlerContext, e : MessageEvent ) : Unit = {
    //log.info("message event! "+this+"message: "+e.getMessage)
    super.messageReceived(ctx,e)
  }

  override def exceptionCaught( ctx : ChannelHandlerContext, e : ExceptionEvent ) : Unit = {
    WaitingRoom ! RemovePatient( ctx.getChannel,peer )
    e.getChannel.close
    super.exceptionCaught(ctx,e)
  }

  override def channelDisconnected( ctx : ChannelHandlerContext, e : ChannelStateEvent ) : Unit = {
    //log.info("channel disconnected "+this)
    WaitingRoom ! RemovePatient( ctx.getChannel,peer )
    super.channelDisconnected(ctx,e)
  }
}