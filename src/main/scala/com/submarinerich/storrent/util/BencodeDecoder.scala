package com.submarinerich.storrent.util

object BencodeDecoder
{
  import java.nio.ByteBuffer
  /* the main mehtod of interaction with this class */
  def decode( data : Array[Byte] ) : Any = {
    if( dictionaryStart( data(0) ) ){
      processDictionary( data ) 
    }else if( listStart( data(0))){
      processList(data)
    }else if(intStart(data(0))){
      processInteger(data)
    }else if(byteStart(data(0))){
      processByte(data)
    }else{
      "empty"
    }
  }

  def infoBytes( data : Array[Byte] ) : Array[Byte] = { //this is a hack, for torrents only
    //this hack deals with the fact that, when you BencodeEncode something, the Map
    //keys are always alphabetical - this gets around that by just excising the 
    // info bytes
  	var infostart = 0
    for( i <- 0 until data.size-5 ){
      if( data(i) == '4'.toByte && 
        data(i+1) == ':'.toByte && 
        data(i+2) == 'i'.toByte && 
        data(i+3) == 'n'.toByte && 
        data(i+4) == 'f'.toByte && 
        data(i+5) == 'o'.toByte)
        infostart = i
    }
    var infoEnd = getEnd(data,infostart)
    var infoBytesStart = infoEnd + 1
    var infoByteEnd = getEnd(data, infoBytesStart)
    subArray( data, infoBytesStart,infoByteEnd+1)
  }

	// byte tests to see what we're starting (we're going byte by byte)

  def byteStart( b : Byte ) : Boolean = {
  	( '1'.toByte == b || 
      '2'.toByte == b ||
      '3'.toByte == b ||
      '4'.toByte == b ||
      '5'.toByte == b ||
      '6'.toByte == b ||
      '7'.toByte == b ||
      '8'.toByte == b ||
      '9'.toByte == b ||
      '0'.toByte == b )
  }

  def intStart( b : Byte ) : Boolean = {
    'i'.toByte == b
  }

  def listStart( b : Byte ) : Boolean = {
    'l'.toByte == b
  }

  def dictionaryStart( b : Byte ) : Boolean = {
    'd'.toByte == b
  }

  def processInteger( data : Array[Byte] ) : Int = {
  	var negative = false
  	var iterator = 1

    if( data(1) == '-'.toByte ){
    	negative = true 
    	iterator += 1
    }

    var s = subArray(data,iterator,data.size -1 ) 
    var r = str(s).toInt

    if( negative ) 
      r *= -1

    r
  }

  /* actually doing the processing */

  def processByteData( data : Array[Byte] ) : Array[Byte] = {
  	var nb = numberBytes( data ) 
    nb._2
  }

  def processByteString( data : Array[Byte] ) : String = {
  	var nb = numberBytes( data ) 
  	str( nb._2 )
  }

  def processByte( data : Array[Byte] ) : Any = {
    if( data.size < 200 )
      processByteString(data)
    else{
      processByteData(data)
    }
  }

  def processList( data : Array[Byte] ) : List[Any] = {
  	var nl = new scala.collection.mutable.ListBuffer[Any]()
    var i = 1

    while( i < data.size -1 ){
      var e = getEnd(data,i)
      var b = data(i)

      if( dictionaryStart( b ) )
        nl += processDictionary( subArray( data, i, e+1 ) )

      if( intStart( b ) )
        nl += processInteger( subArray( data, i , e+1 ))

      if( listStart( b ))
        nl += processList( subArray(data,i,e+1 ))

      if( byteStart( b ))
        nl += processByte( subArray(data,i,e+1 ))

      i = e + 1
    }
    nl.toList
  }

  def processDictionary( data : Array[Byte] ) : Map[String,Any] = {
    var i = 1
    var k = true
    var key = "" 
    var hm = new scala.collection.mutable.HashMap[String,Any]()
    while( i < data.size - 1){
        var e = getEnd(data,i)
        var stringbit = subArray(data,i,e+1)
        var nb = numberBytes(stringbit)
        i = e + 1
        key = str(nb._2)
        var valueEnd = getEnd(data,i)
        var valueBytes = subArray(data,i,valueEnd+1)

        if( listStart( valueBytes(0) ) )
          hm += (key -> processList(valueBytes) )

        if( dictionaryStart( valueBytes(0)))
          hm += (key -> processDictionary( valueBytes) )

        if( intStart( valueBytes(0) ))
          hm += (key -> processInteger( valueBytes ) )

        if( byteStart( valueBytes(0) ) )
          hm += (key -> processByte( valueBytes ) )
          
        i = valueEnd + 1
    }
    hm.toMap
  }

  /* util methods */
  def str ( data : Array[Byte] ) : String = {
    new String( data.map(_.toChar) )
  }

  def getEnd( data : Array[Byte], index : Int ) : Int = { 
    //string
    var i = index

    if( byteStart(data(i)) ){
      while( data(i) != ':'.toByte )
        i += 1
      var n = new scala.collection.mutable.ArrayBuffer[Byte]()
      for( a <- index until i )
        n += data(a)
      var num = str(n.toArray).toInt
      (i+num)
    }else if( intStart( data(i) ) ){
    	while( data(i) != 'e'.toByte )
        i += 1
      i
    }else if( listStart( data(i) ) ){
    	i += 1
      while( data(i) != 'e'.toByte ){
       i = ( getEnd(data,i) )
       i += 1
     }
      i
    }else if( dictionaryStart( data(i) ) ){
    	i += 1
      while( data(i) != 'e'.toByte ){
        i = ( getEnd(data,i) )
        i += 1
      }
      i
    }else{
      -1
    }
  }

  def subArray( data : Array[Byte] , start : Int, end : Int ) : Array[Byte] = {
    var b = ByteBuffer.allocate( end - start )
    for( i <- 0 until (end - start) )
      b.put( i, data( start + i) )
    b.array()
  }

  def numberBytes( data : Array[Byte] ) : Tuple2[Int,Array[Byte]] = {
    var iterator = 0
    while( data( iterator ) != ':'.toByte )
      iterator += 1
    //new String( source.map(_.toChar)
    var number = new Array[Byte]( iterator ) 
    for( i <- 0 until iterator)
      number(i) = data(i)
    iterator+= 1
    var data1 = new Array[Byte]( data.size - iterator )
    for( i <- iterator until data.size )
      data1(i - iterator) = data(i)
    var r = (new String( number.map(_.toChar) )).toInt
    (r,data1)
  }
}