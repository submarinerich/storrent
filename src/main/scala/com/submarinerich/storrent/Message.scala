package com.submarinerich.storrent

import util.ByteUtils
import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.buffer.ChannelBuffers
import grizzled.slf4j.Logger

object MessageType extends Enumeration
{
	type MessageType = Value
  val KeepAlive,Choke,Unchoke,Interested,NotInterested,Have,Bitfield,Request,Piece,Cancel,Undefined = Value
}

case class InsufficientDataException() extends Exception

case class Message( messageContent : Array[Byte] )
{
	import MessageType._
  import scala.collection.mutable.ArrayBuffer

	var log = Logger(classOf[Message])
	var msgType : MessageType = {
		  //log.info("parsing: "+ByteUtils.hexString(buffer.array()))
		  if( messageContent.size == 4 ){
        var t = messageContent
        var ka = true
        for( t1 <- t )
          if( t1 != 0x00 )
            ka = false
        if( ka )
          KeepAlive
        else
          Undefined
      }else{
        var idByte : Byte = messageContent(4)
		    if( idByte == 0x05 )
			  	Bitfield
			  else if( idByte == 0x04 ){
			  	Have
			  }else if( idByte == 0x00 ){
			  	Choke
			  }else if( idByte == 0x01 ){
			  	Unchoke
			  }else if( idByte == 0x02 ){
			  	Interested
			  }else if( idByte == 0x03 ){
			  	NotInterested
			  }else if( idByte == 0x06 ){
			  	Request
			  }else if( idByte == 0x07 ){
			  	Piece
			  }else if( idByte == 0x08){
			  	Cancel
			  }else{
			  	Undefined
			  }
      }
	}

  def dataChunk : Option[Tuple3[java.nio.ByteBuffer,Int,Int]] = {
    // <len=0009+X><id=7><index><begin><block>
    if( msgType == Piece ){
    	import util.ByteUtils

      var ra : Array[Byte] = new Array[Byte](4)

      for( i <- 0 until 4 )
        ra(i) = messageContent(i)

      var lengthPlus9 = ByteUtils.toInt(ra)
      var length = lengthPlus9 - 9
      
      for( i <- 0 until 4 )
        ra(i) = messageContent( 5 + i )
      
      var index = ByteUtils.toInt(ra)

      for( i <- 0 until 4 )
        ra(i) = messageContent( 9 + i )

      var begin = ByteUtils.toInt(ra)
      
      var returnBuffer = java.nio.ByteBuffer.allocate(length)
      returnBuffer.clear

      for( i <- 0 until length )
        returnBuffer.put(messageContent(i+13))

      returnBuffer.flip

      Some( (returnBuffer,index,begin) )
    }else
      None
  }

	private var bitfieldPieces : ArrayBuffer[Int] = new ArrayBuffer[Int]()
	def pieces : Array[Int] = bitfieldPieces.toArray
      
	var bitfieldLength : Option[Int] = {
    if( msgType == Bitfield ){
      import util.ByteUtils
      var ra : Array[Byte] = new Array[Byte](4)
      for( i <- 0 until 4 )
        ra(i) = messageContent(i)
      var lengthplus1 = ByteUtils.toInt(ra)
      var length = lengthplus1 - 1
      import java.util.BitSet
      var bs = new BitSet( length * 8 ) // 8 bits in a byte
      for( i <- 0 until length ){
      	var b : Byte = messageContent(5+i)
      	var bitNumberStart = (i * 8)
        import java.lang.Math.pow
        for( j <- 0 until 8 ){
          if( (b & pow(2,7-j).asInstanceOf[Int]) == 0 )
          	bs.set(bitNumberStart+j,false)
          else{
            bs.set(bitNumberStart+j)
          }
        }
      }
      // for(int i=bs.nextSetBit(0); i>=0; i=bs.nextSetBit(i+1)) { // operate on index i here }
      var i : Int = bs.nextSetBit(0)
      while( i >= 0 ){
        bitfieldPieces += i
        i = bs.nextSetBit(i+1)
      }
      Some( length )
    }else
      None
  }

	var pieceIndex : Option[Int] = {
		if( msgType == Have ){
			var ra : Array[Byte] = new Array[Byte](4)
      if( messageContent.size != 9 )
        throw new InsufficientDataException()
			for( i <- 0 until 4)
				ra(i) = messageContent(5+i)
			Some(ByteUtils.toInt(ra))
		}else
			None
	}

	var requestParams : Option[Tuple3[Int,Int,Int]] = { // index, begin, length
		if( msgType == Request ){
			import util.ByteUtils

      if( messageContent.size != 17 )
        throw new InsufficientDataException()

			var ra : Array[Byte] = new Array[Byte](4)
			for( i <- 0 until 4)
				ra(i) = messageContent(5+i)
			var index : Int = ByteUtils.toInt(ra)
			for( i <- 0 until 4)
				ra(i) = messageContent(9+i)
			var begin : Int = ByteUtils.toInt(ra)
			for( i <- 0 until 4)
				ra(i) = messageContent(13+i)
			var length : Int = ByteUtils.toInt(ra)
			Some((index,begin,length))
		}else
			None
	}

	var pieceParams : Option[Tuple2[Int,Int]] = { // index, begin, blok
		if( msgType == Piece ){
			import util.ByteUtils
			var ra : Array[Byte] = new Array[Byte](4)
			for( i <- 0 until 4)
				ra(i) = messageContent(5+i)
			var index : Int = ByteUtils.toInt(ra)
			for( i <- 0 until 4)
				ra(i) = messageContent(9+i)
			var begin : Int = ByteUtils.toInt(ra)
			Some((index,begin ))
		}else
			None
	}
	
	override def toString = {
		msgType match {
			case Have => {"[MSG]: "+msgType+" piece: "+pieceIndex.get}
			case Piece => {
				var pp = pieceParams.get
				"[MSG]: "+msgType+" "+" index: "+pp._1+" begin: "+pp._2
			}
			case Request => {
				var pp = requestParams.get
				"[MSG]: "+msgType+" "+" index: "+pp._1+" begin: "+pp._2+" length: "+pp._3
			}
			case _ => {"[MSG]: "+msgType}
		}
	}
}


object Message
{
  import util.ByteUtils

	var log = Logger("Message")
  def keepAlive : ChannelBuffer = {
    var ka = ChannelBuffers.buffer(4)
    for( i <- 0 until 4 )
      ka.writeByte(0x00)
    ka
  }
  //choke: <len=0001><id=0>
  //The choke message is fixed-length and has no payload.

  def choke : ChannelBuffer = {
    var ch = ChannelBuffers.buffer(5)
    for( i <- 0 until 3 )
      ch.writeByte(0x00)
    ch.writeByte(0x01)
    ch.writeByte(0x00)
    ch
  }

  //unchoke: <len=0001><id=1>
  //The unchoke message is fixed-length and has no payload.
  
  def unchoke : ChannelBuffer = {
    var ch = ChannelBuffers.buffer(5)
    for( i <- 0 until 3 )
      ch.writeByte(0x00)
    ch.writeByte(0x01)
    ch.writeByte(0x01)
    ch
  }
  
  //interested: <len=0001><id=2>
  //The interested message is fixed-length and has no payload.
  
  def interested : ChannelBuffer = {
    var ch = ChannelBuffers.buffer(5)
    for( i <- 0 until 3 )
      ch.writeByte(0x00)
    ch.writeByte(0x01)
    ch.writeByte(0x02)
    ch
  }
  
  //not interested: <len=0001><id=3>

  def notInterested : ChannelBuffer = {
    var ch = ChannelBuffers.buffer(5)
    for( i <- 0 until 3 )
      ch.writeByte(0x00)
    ch.writeByte(0x01)
    ch.writeByte(0x03)
    ch
  }

  def bitfield( t : Torrent ) : ChannelBuffer = {
    import java.util.BitSet

    var bs = new BitSet(t.pieces.size)

    for( a <- 0 until t.pieces.size )
      if( t.pieces(a).confirmed )
        bs.set(a)
    
    var bytes : Array[Byte] = new Array[Byte](bs.length/8+1)

    for( i <- 0 until bs.length ) 
      if(bs.get(i)){
        var b : Byte = (1 << (i % 0x08)).toByte
        bytes(bytes.length-i/8-1) = (bytes(bytes.length-i/8-1) | b).toByte
      }

    var length : Int = (t.pieces.size + 7) / 8
    var cb = ChannelBuffers.buffer( length + 5 )
    var len = ByteUtils.toByteArray(length + 1 )

    for( a <- len )
      cb.writeByte(a)

    cb.writeByte(0x05)

    for( a <- bytes )
      cb.writeByte(a)

    log.info("bitfield looks like: "+ByteUtils.hexString(cb.array))
    cb
  }

	/*
  request: <len=0013><id=6><index><begin><length>
  The request message is fixed length, and is used to request a block. The payload contains the following information:
  index: integer specifying the zero-based piece index
  begin: integer specifying the zero-based byte offset within the piece
  length: integer specifying the requested length.
  */

  val requestSize : Int = 4 + 1 + 12

	def request( index : Int, begin : Int, length : Int ) : Array[Byte] = {
		var ind = ByteUtils.toByteArray(index)
		var beg = ByteUtils.toByteArray(begin)
		var len = ByteUtils.toByteArray(length)
		var ch = ChannelBuffers.buffer(requestSize)

		for( a <- 0 until 3 )
      ch.writeByte(0x00)

    ch.writeByte(0x0d)
    ch.writeByte(0x06)
  
		for( i <- 0 until 4)
      ch.writeByte(ind(i))

		for( i <- 0 until 4)
      ch.writeByte(beg(i))

		for( i <- 0 until 4)
      ch.writeByte(len(i))

    //log.info("request message: "+ByteUtils.hexString(ch)
		ch.array
	}

	def requestMultiple( poplist : List[PieceOfPiece] ) : ChannelBuffer = {
    var ch = ChannelBuffers.buffer( requestSize * poplist.size )
    
    poplist.map( a => {
      var n = request(a.index,a.start,a.size)
      for( nb <- n )
        ch.writeByte(nb)
    })

    ch
  }
}