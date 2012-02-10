package com.submarinerich.storrent.util

object UnicodeFormatter {

	def byteArrayToHex( b : Array[Byte] ) : String = {
		ByteUtils.hexString(b)
	}
	
	def escapedBytes( b : Array[Byte] ) : String = {
		import scala.collection.mutable.ArrayBuffer

		var n : ArrayBuffer[String] = new ArrayBuffer[String]()

		b.map( a => n += (ByteUtils.byteToHex(a)).toUpperCase)
		n.mkString("%","%","")
	}
	
	def urlEncodedEscapedBytes( b : Array[Byte] ) : String = {
		import java.net.URLEncoder
		
		URLEncoder.encode(escapedBytes(b))
	}
}
