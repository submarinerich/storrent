package test.scala

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable.ArrayBuffer
import grizzled.slf4j.Logger

class NewBencoderTestSuite extends FunSuite with ShouldMatchers {

	var log = Logger(classOf[NewBencoderTestSuite] )

  import com.submarinerich.storrent.util.BencodeDecoder

  def stringToBytes( str : String ) : Array[Byte] = {
    var ral = new scala.collection.mutable.ArrayBuffer[Byte]()
    for( i <- 0 until str.size )
      ral += str(i).toByte
    ral.toArray
  }
  
  test("test the hacky infoBytes method "){
    var b = stringToBytes("4:infod2:ab1:ee")
    var v = BencodeDecoder.infoBytes( b )
    v.size should equal (9)
  }

  test("test getting the end of things"){
    var ge = BencodeDecoder.getEnd(Array('i'.toByte, '1'.toByte,'2'.toByte,'e'.toByte), 0 )
    ge should equal(3)
    var ge1 = BencodeDecoder.getEnd(Array('l'.toByte,'i'.toByte, '1'.toByte,'2'.toByte,'e'.toByte,'e'.toByte), 0 )
    ge1 should equal(5)
    var complicatedDictionary = stringToBytes("d5:title8:Darkness3:key5:valuee")
    BencodeDecoder.getEnd( complicatedDictionary, 1 ) should equal (7)
    BencodeDecoder.getEnd( complicatedDictionary, 0 ) should equal (30)
    var nb = BencodeDecoder.numberBytes(Array('2'.toByte,':'.toByte,'a'.toByte, 'b'.toByte) )  
    //log.info("nb: "+nb)
    nb._1 should equal (2)
  }

  test("test bencoding of the first chunk of a torrent file"){
  	
  	import java.io.FileInputStream
  	import org.apache.commons.io.IOUtils

    var fis = new FileInputStream( "src/test/resources/bencodetest.txt" )
    val source = IOUtils.toByteArray(fis) 

    source.size should be > (100)
    var results = BencodeDecoder.decode( source ) 
    //log.info(" results from decoding: "+results)
  }

  import com.submarinerich.storrent.util.BencodeEncoder
  test("test bencode encode "){
    var b = BencodeEncoder.encode(243)
    b.size should equal (5)

    var b1 = BencodeEncoder.encode("testinger")
    b1.size should equal (11)
  }

  test("make sure an encode and a decode have the same result"){
  
  	import java.io.FileInputStream
  	import org.apache.commons.io.IOUtils

    var fis = new FileInputStream( "src/test/resources/bencodetest.txt" )
    val source = IOUtils.toByteArray(fis) 

    source.size should be > (100)
    var results = BencodeDecoder.decode( source ) 
    //log.info(" results from decoding: "+results)
    var source2 = BencodeEncoder.encode(results)
    source.size should equal (source2.size)
    //log.info(" source "+BencodeDecoder.str(source))
    //log.info(" source2 "+BencodeDecoder.str(source2))
  }

  test("test reading a torrent file"){

  	import java.io.FileInputStream
  	import org.apache.commons.io.IOUtils

    var fis = new FileInputStream( "src/test/resources/linuxmint12.torrent" )
    val source = IOUtils.toByteArray(fis) 
    var results = (BencodeDecoder.decode( source )).asInstanceOf[Map[String,Any]] 
    results.keys.size should be >= (5)
    //log.info(" results from decoding: "+results)
  }
}