package test.scala

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll
import grizzled.slf4j.Logger

class FileTestSuite extends FunSuite with ShouldMatchers {
	
  import com.submarinerich.storrent.util.ByteUtils

  var log = Logger("FileTestSuite")

  import java.nio.ByteBuffer
  import java.io.FileInputStream
  import java.io.File
  import java.io.FileOutputStream

  import java.nio.ByteBuffer
  import java.nio.channels.FileChannel
	import org.apache.commons.io.FileUtils

  test("make a giant file and then read part of into a byte buffer"){
  	var testFile = new File("/tmp/test.txt")
  	testFile.exists should equal (true)
  	FileUtils.forceDelete(testFile)
  	testFile.exists should equal (false)

  	var byteToFillWith : Byte = 0x07

		var b = ByteBuffer.allocate( 256 ) 
    for( a <- 0 until 256 )
        b.put(a,byteToFillWith)
      
    b.rewind
    b.array.size should equal (256)

    var fileChannel = new java.io.FileOutputStream(testFile).getChannel
    for( a <- 0 until 1024*1024 ){
    	fileChannel.write(b)
      b.rewind
    }
    fileChannel.close

    testFile.exists should equal (true)

    fileChannel = new java.io.FileInputStream(testFile).getChannel
    fileChannel.size should equal (1024*1024*256)
    var ob = ByteBuffer.allocate( 7240 )
    ob.clear
		fileChannel.position(24080)
    fileChannel.read(ob)
    var readProperly = true
    var instead = 0x02
    ob.array.map(a => {
	    if( a != byteToFillWith ){
     		readProperly = false 
     		instead = a 
     	}
     })
     log.info("instead: "+instead)
    readProperly should equal (true)
  }
  /*test("test 2 with real thing"){
  	var testFile = new File("/Users/andersonmiller/Documents/storrent/download/partialtorrent.part")
  	testFile.exists should equal (true)
  	var fileChannel = new java.io.FileInputStream(testFile).getChannel
    fileChannel.size should be >= (9863168L)
    log.info("filechannel size: "+fileChannel.size)
  }*/
}