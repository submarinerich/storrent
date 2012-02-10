package com.submarinerich.storrent.util

case class InvalidByteArgumentException() extends Exception

object ByteUtils
{  
  def toByteArray( l : Long ) : Array[Byte] = {
    var ra : Array[Byte] = Array(0,0,0,0,0,0,0,0)
    for( i <- 0 until 8) 
      ra( 7 - i ) = ( l >>>  (i*8)).toByte
    ra
  }

  def toByteArray( l : Int ) : Array[Byte] = {
    var ra : Array[Byte] = Array(0,0,0,0)
    for( i <- 0 until 4) 
      ra( 3 - i ) = ( l >>> (i*8)).toByte
    ra
  }

  //<! TODO - make a toLong( ra : Array[Byte] ) : Long method

  def toInt( ra : Array[Byte] ) : Int = {
  	 if( ra.size == 0 || ra.size > 4 )
       throw new InvalidByteArgumentException()

  	 var byteArray : Array[Byte] = Array()

     if( ra.size == 4 )
      byteArray = ra
     else if( ra.size == 3 ){
      byteArray = Array( 0x00, ra(0), ra(1), ra(2) )
     }else if(ra.size == 2 ){
      byteArray = Array( 0x00, 0x00, ra(0), ra(1) )
     }else if( ra.size == 1 ){
      byteArray = Array( 0x00, 0x00, 0x00, ra(0) )
     }
     ((byteArray(0) << 24) + ((byteArray(1) & 0xFF) << 16) + ((byteArray(2) & 0xFF ) << 8 ) + (byteArray(3) & 0xFF)).toInt
  }

  def hexString( ra : Array[Byte] ) : String = {
		import scala.collection.mutable.ArrayBuffer
		
    var n : ArrayBuffer[String] = new ArrayBuffer[String]()
		ra.map( a => n += byteToHex(a))
		n.mkString
  }

  def byteToHex( b : Byte ) : String = {
    var hexDigit : List[Char] = List('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f' )
    (hexDigit(b >> 4 & 0x0f)).toString + (hexDigit(b & 0x0f)).toString  
  }
}