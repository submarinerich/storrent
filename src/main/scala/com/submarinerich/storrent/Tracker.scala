package com.submarinerich.storrent

import java.net.URL
import java.net.URI
import java.net.InetSocketAddress

import util.UnicodeFormatter
import util.ByteUtils
import com.twitter.conversions.time._
import com.twitter.util.Time
import grizzled.slf4j.Logger

case class HostnameNotFoundException() extends Exception
case class InvalidProtocolException() extends Exception
case class ScrapeResponse( torrentHex : String, seeders : Int, completed : Int, leechers : Int )

object AnnounceEvent extends Enumeration
{
  type Event = Value
  val NoneEvent, CompletedEvent, StartedEvent, StopEvent = Value
}

object Protocol extends Enumeration
{
  type ProtocolType = Value
  val HTTP,UDP = Value
}

case class Tracker( urlstring : String )
{
  import Protocol._
  var log = Logger(classOf[Tracker])
  var address : String = urlstring

  def hostname : String = {
    var two : Array[String] = address.split("://")

    if( two.size != 2 )
      throw new HostnameNotFoundException()

    if( two(1).split(":")(0).size < two(1).size )
      two(1).split(":")(0)

    else
      two(1).split("/")(0)
  }

  def uri : URI = {
    var two : Array[String] = address.split("://")
    
    if( two(1).split("/").size >= 2 )
      new URI( "/"+two(1).split("/")(1) )

    else
      new URI("/")
  }
  def protocol : ProtocolType = {
    var pt : String = urlstring.split("://")(0)

    if( pt == "udp" )
      UDP

    else
      HTTP
  }
  def port : Int = {
    if( urlstring.split(":").size > 2 ){
      var pn : String = urlstring.split(":")(2).split("/")(0)

      if(pn.length > 0)
        pn.toInt

      else
        80
      
    }else
      80
  }

  var scrapeResponses : scala.collection.mutable.ListBuffer[ScrapeResponse] = new scala.collection.mutable.ListBuffer[ScrapeResponse]()

  def socketAddress : InetSocketAddress = new InetSocketAddress(java.net.InetAddress.getByName( hostname ),port)

  private var random = new scala.util.Random( Time.now )

  import java.util.Date
  import scala.util.Random
  import java.net.InetAddress
  import AnnounceEvent._
  import org.jboss.netty.buffer.ChannelBuffer
  import org.jboss.netty.buffer.ChannelBuffers

  def connectRequestBuffer : Tuple2[ChannelBuffer,Int] = {
    var cb = ChannelBuffers.buffer(16)
    var transaction : Int = java.lang.Math.abs(random.nextInt())
    var transaction_id : Array[Byte] = ByteUtils.toByteArray(transaction)

    for( i <- 0 until 12)
      cb.writeByte(0x00)
    
    for( i <- transaction_id )
      cb.writeByte(i)

    (cb,transaction)
  }

  def scrapeRequestBuffer( t : Torrent ) : Tuple2[ChannelBuffer,Int] = {
    var cb = ChannelBuffers.buffer(36)
    var connection_id : Array[Byte] = ByteUtils.toByteArray(41727101980L)
    var transaction : Int = java.lang.Math.abs(random.nextInt())
    var transaction_id : Array[Byte] = ByteUtils.toByteArray(transaction)

    for( i <- connection_id )
      cb.writeByte(i)

    for( i <- 0 until 3)
      cb.writeByte(0x00)

    cb.writeByte(0x02)

    for( i <- transaction_id )
      cb.writeByte(i)

    val info_hash = t.info_hash
    
    for( i <- info_hash )
      cb.writeByte(i)

    (cb,transaction)
  }


  def announceRequestBuffer( t: Torrent, peer_id_string : String, port : Int )
                           (implicit event : Event = NoneEvent, downloaded : Long = 0L, uploaded : Long = 0L, left : Long = 0L ) : Tuple3[ChannelBuffer,Int,Int] = { 
                           //send back the packet and the transaction
    var cb = ChannelBuffers.buffer(98)
    var connection_id : Array[Byte] = ByteUtils.toByteArray(41727101980L) // 8 bytes
    var transaction : Int = java.lang.Math.abs(random.nextInt())
    var transaction_id : Array[Byte] = ByteUtils.toByteArray(transaction) // 4 bytes
    var info_hash = t.info_hash //20 bytes
    var peer_id = peer_id_string.getBytes() //20 bytes
    cb.clear

    for( i <- connection_id )
      cb.writeByte(i)

    for( i <- 8 until 11)
      cb.writeByte(0x00)

    cb.writeByte(0x01)

    for( i <-  transaction_id )
      cb.writeByte(i)

    for( i <- info_hash)
      cb.writeByte(i)

    for( i <- peer_id)
      cb.writeByte(i)

    var downloaded_bytes = ByteUtils.toByteArray(downloaded)
    for( i <- downloaded_bytes )
      cb.writeByte(i) // downloaded

    var sendLeft : Long = 0L
    if( left == 0L ){
      for( b <- t.files )
        sendLeft += b.length
    }else
      sendLeft = left
    
    var left_bytes = ByteUtils.toByteArray(sendLeft)

    for( i <- left_bytes )
      cb.writeByte(i)

    var uploaded_bytes = ByteUtils.toByteArray(uploaded)

    for( i <-  uploaded_bytes) //uploaded
      cb.writeByte(i) 

    //event
    //val NoneEvent, CompletedEvent, StartedEvent, StopEvent = Value
    //80      32-bit integer  event           0 // 0: none; 1: completed; 2: started; 3: stopped
    for( i <- 0 until 3 ) //in any of these cases, 80, 81, 82 are going to be 0x00
      cb.writeByte(0x00) 

    event match {
      case NoneEvent => cb.writeByte(0x00)
      case CompletedEvent => cb.writeByte(0x01)
      case StartedEvent => cb.writeByte(0x02)
      case StopEvent => cb.writeByte(0x03)
      case _ => cb.writeByte(0x00)
    }

    //ip address
    for( i <- 0 until 4 ) 
      cb.writeByte(0x00)

    //key
    var key = random.nextInt()
    var keyra = ByteUtils.toByteArray(key)

    for( i <- keyra )
      cb.writeByte(i)

    var n1 : Int = -1
    var n1b = ByteUtils.toByteArray(n1)

    for( i <-  n1b )//num_want
      cb.writeByte(i)
    
    var pb = ByteUtils.toByteArray(port)
    cb.writeByte(pb(2))
    cb.writeByte(pb(3))

    log.debug("made data: "+UnicodeFormatter.byteArrayToHex(cb.array()))

    (cb,transaction,key)
  }

  override def toString : String = hostname.toString + " " + uri.toString + "  "  + protocol.toString+" port : "+port.toString

}