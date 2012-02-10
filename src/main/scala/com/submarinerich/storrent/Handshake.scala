package com.submarinerich.storrent

import util.ByteUtils

case class InvalidHandshakeLengthException() extends Exception

case class Handshake( pstrLen1 : Int, info_hash1 : Array[Byte], peer_id1 : Array[Byte] )
                    ( implicit reserved1 : Array[Byte] = Array( 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ), pstr1 : Array[Byte] = ("BitTorrent protocol").getBytes() ){

  var pstrlen : Int = pstrLen1
  var pstr : Array[Byte] = pstr1
  var reserved : Array[Byte] = reserved1
  var info_hash : Array[Byte] = info_hash1
  var peer_id : Array[Byte] = peer_id1

  override def toString : String = {
    "[handshake] "+pstrlen+" info: "+ByteUtils.hexString( info_hash )+" peer: "+ByteUtils.hexString( peer_id )    
  }
}

object Handshake
{
  def parse( bytes : Array[Byte] ) : Handshake = {
    var iterator : Int = 0
    var p1 : Int = bytes(0).toInt
    //Console.println(" byte size : "+bytes.size)
    if( bytes.size != 49 + p1 ) //49 being the default length without pstrlen
      throw new InvalidHandshakeLengthException()

    iterator += 1 + p1

    //if we cared about reserved, this is where we'd do it
    iterator += 8

    var infohash : Array[Byte] = new Array[Byte](20)
    var peerid : Array[Byte] = new Array[Byte](20)

    for( i <- 0 until 20)
      infohash(i) = bytes( iterator + i )

    iterator += 20

    for( i <- 0 until 20 )
      peerid(i) = bytes( iterator + i )

    new Handshake( p1, infohash, peerid )
  }
}