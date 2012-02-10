package test.scala

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll
import Console.{ println => debug }

import com.submarinerich.storrent.util.{BencodeEncoder,BencodeDecoder}
import scala.io.Source
import grizzled.slf4j.Logger
import java.io.FileInputStream
import org.apache.commons.io.IOUtils

class TestBencoding extends FunSuite with ShouldMatchers with BeforeAndAfterAll {

  var log = Logger(classOf[TestBencoding])
  override def beforeAll(){
    debug("hey guys")
  }

  override def afterAll(){
    debug("supppp")    
  }

  test("test something first"){
    1 + 1 should equal (2)
  }

  test("decode torrent bencoding"){
  	val fis = new FileInputStream("src/test/resources/linuxmint12.torrent")
    val source = IOUtils.toByteArray(fis)
    val myObject = BencodeDecoder.decode( source )  
    //debug("printing object: "+myObject)
    var contents : Map[String,Any] = myObject.asInstanceOf[Map[String,Any]]
    contents.keys should contain ("announce")
    contents.keys should contain ("creation date")
    contents.keys should contain ("announce-list")
    contents.keys.size should equal (6)
    debug("torrent keys: "+contents.keys)
  }

  import com.submarinerich.storrent.Torrent
	import com.submarinerich.storrent.util.UnicodeFormatter
	
  test("torrent creation object"){
    var t : Torrent = Torrent.fromLocalTest("/linuxmint12.torrent")    
    t.creationDate.getTime() should be < ( (new java.util.Date()).getTime() )
    t.announceList.size should be > (1)
    for( i <- t.announceList )
      debug(" tracker: "+i.toString)
    debug("torrent :"+t.toString)
		debug("torrent info hash "+UnicodeFormatter.byteArrayToHex(t.info_hash))
		debug("torrent info hash escaped " + UnicodeFormatter.escapedBytes( t.info_hash ))
    // for( a <- t.pieces )
    //   debug("this: "+a.toString )
  }
}