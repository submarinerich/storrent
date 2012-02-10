package com.submarinerich.storrent.util

object BencodeEncoder
{
	import java.nio.ByteBuffer

  def encode( o : Any ) : Array[Byte] = {
    o match {
      case l : List[Any] => encodeList(l)
      case i : Int => encodeInteger(i)
      case d : Map[String,Any] => encodeDictionary(d)
      case b : String => encodeString( b )
      case b : Array[Byte] => encodeByteArray( b )
      case _ => Array()
    }
  }

  def encodeList( l : List[Any] ) : Array[Byte] = {
  	var ra : Array[Byte] = Array()

    for( i <- l ){
      var new1 = encode(i)
      var bb = ByteBuffer.allocate(ra.size + new1.size )
      for( i1 <- 0 until ra.size )
        bb.put(i1,ra(i1))
      for( i2 <- 0 until new1.size )
        bb.put(ra.size+i2,new1(i2))
      ra = bb.array()
    }

    var bb1 = ByteBuffer.allocate(ra.size+2)
    bb1.put(0,'l'.toByte)

    for( i3 <- 0 until ra.size )
      bb1.put(i3+1, ra(i3))

    bb1.put(ra.size+1,'e'.toByte)
    bb1.array() 
  }

  def encodeInteger(i : Int ) : Array[Byte] = {
    stringToBytes("i"+ i.toString+"e")
  }

  def encodeDictionary( d : Map[String,Any] ) : Array[Byte] = {
  	var ra : Array[Byte] = Array()

  	for( i <- d.keys){
      var newKey = encode(i)
      var value = d(i)
      var newValue = encode(value)
      var bb = ByteBuffer.allocate(ra.size + newKey.size + newValue.size )
      for( i1 <- 0 until ra.size )
        bb.put(i1,ra(i1))
      for( i2 <- 0 until newKey.size )
        bb.put(ra.size+i2,newKey(i2))
      for( i3 <- 0 until newValue.size )
        bb.put(ra.size+newKey.size+i3,newValue(i3))
      ra = bb.array()
    }
    var bb1 = ByteBuffer.allocate(ra.size+2)
    bb1.put(0,'d'.toByte)

    for( i4 <- 0 until ra.size )
      bb1.put(i4+1, ra(i4))

    bb1.put(ra.size+1,'e'.toByte)
    bb1.array() 
  }

  def encodeString( s : String ) : Array[Byte] = {
    stringToBytes(s.length.toString + ":"+s) 
  }

  def encodeByteArray( b : Array[Byte] ) = {
    var l = b.size.toString
    var ra = ByteBuffer.allocate(l.length+1+b.size)
    var lengthAsArray = stringToBytes(l)

    for( i <- 0 until lengthAsArray.size)
      ra.put(i,lengthAsArray(i))

    ra.put(lengthAsArray.size,':'.toByte)
    var iter = lengthAsArray.size+1

    for( i <- 0 until b.size )
      ra.put(iter+i,b(i))

    ra.array()
  }

  private def stringToBytes( str : String ) : Array[Byte] = {
    var ral = ByteBuffer.allocate(str.length)

    for( i <- 0 until str.size )
      ral.put(i,str(i).toByte)

    ral.array()
  }
}