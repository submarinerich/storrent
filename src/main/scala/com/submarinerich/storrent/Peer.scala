package com.submarinerich.storrent

import util.ByteUtils
import com.twitter.conversions.time._
import com.twitter.util.Time
import java.net.InetAddress
import java.util.Date
import grizzled.slf4j.Logger

object AmState extends Enumeration
{
  type AmState = Value
  val AmChoking,AmInterested = Value
}
 
object PeerState extends Enumeration
{
  type PeerState = Value
  val PeerChoking,PeerInterested = Value
}

case class NotImplementedException() extends Exception

case class Peer( addressbytes : Array[Byte], portnumber : Array[Byte] , t : Torrent, myPeerId : String ) 
{
  import org.jboss.netty.buffer.{ChannelBuffer,ChannelBuffers}

	var log = Logger(classOf[Peer])
  private var peer_id = TorrentClient.peer_id 
  var address : InetAddress = InetAddress.getByAddress(addressbytes)
  var host : String = address.getHostAddress()	
  var port : Int = ByteUtils.toInt( portnumber )
  var peerId : Option[Array[Byte]] = None
  var torrent : Torrent = t
  var connected : Boolean = false

  override def toString : String = "host: "+host+" port: "+port

  def makeHandshake : ChannelBuffer = {
    val pstrlen : Byte = 0x13 // 1 //holy damnit 0x13 [IS] equal to 19
    val pstr : Array[Byte] = ("BitTorrent protocol").getBytes() // 19
    val reserved : Array[Byte] = Array( 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ) // 8
    val info_hash : Array[Byte] = torrent.info_hash // 20
    var pid : Array[Byte] = peer_id.getBytes()  //20
     
    var handshakeBytes : ChannelBuffer = ChannelBuffers.buffer(68)//68

    handshakeBytes.writeByte(pstrlen)

    for( i <- 0 until 19 )
      handshakeBytes.writeByte(pstr(i))

    for( i <- 0 until 8 )
      handshakeBytes.writeByte(reserved(i))

    for( i <- 0 until 20)
      handshakeBytes.writeByte( info_hash(i) )

    for( i <- 0 until 20 )
      handshakeBytes.writeByte( pid(i) )
    //log.info("handshake looks like "+ByteUtils.hexString( handshakeBytes.array )) 
    handshakeBytes
  }
}