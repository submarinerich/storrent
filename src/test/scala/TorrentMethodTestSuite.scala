package test.scala

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll
import Console.{ println => debug }
import java.io.BufferedInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.net.InetSocketAddress
import java.io.File
import java.net.URL
import java.net.URLConnection
//import org.saunter.bencode._
import scala.io.Source

class TorrentMethodTestSuite extends FunSuite with ShouldMatchers with BeforeAndAfterAll {

  override def beforeAll(){
    
  }

  override def afterAll(){
      
  }

	/*
	var t : Torrent = Torrent.load(new File(d),new File("/tmp/test/"))
	*/
	def download( local : File, url : String ) : String = {
		var u : URL = new URL(url)
	  var uc : URLConnection = u.openConnection()
	  var contentType : String = uc.getContentType()
	  var contentLength : Int = uc.getContentLength()
	  if (contentType.startsWith("text/") || contentLength == -1) {
	    throw new IOException("This is not a binary file.")
	  }
	  var raw : InputStream = uc.getInputStream()
	  var in : InputStream = new BufferedInputStream(raw)
	  var data : Array[Byte] = new Array[Byte](contentLength)//[contentLength]
	  var bytesRead : Int = 0;
	  var offset : Int = 0;
	  while (offset < contentLength) {
	    bytesRead = in.read(data, offset, data.length - offset);
	    if (bytesRead == -1){

	    }else{
	    	offset += bytesRead
	    }
	  }
	  in.close();

	  if (offset != contentLength) {
	    throw new IOException("Only read " + offset + " bytes; Expected " + contentLength + " bytes");
	  }

	  var filepathitems : Array[String] = u.getFile().split('/')

	  var filename : String = filepathitems(filepathitems.size-1)
	  var out : FileOutputStream = new FileOutputStream(local.toString+"/"+filename);
	  out.write(data);
	  out.flush();
	  out.close();
	  local.toString+"/"+filename
	}
/*
	import com.turn.ttorrent.common.{ Torrent => t1 }
	import com.turn.ttorrent.client.Client
	import com.turn.ttorrent.client.Client.ClientState
	import com.turn.ttorrent.client.SharedTorrent

	import java.net.InetAddress

	import com.submarinerich.storrent.Torrent
	import com.submarinerich.storrent.util.UnicodeFormatter
	test("testing hashability with ttorrent"){
		var url : String = RandomTorrent.get 
		var d = download(new File("/tmp/test/"),url)
		var t : t1 = t1.load(new File(d),new File("/tmp/test/"))
		
		var srtorrent : Torrent = Torrent.fromURL(url)
		var ourhash = UnicodeFormatter.byteArrayToHex(srtorrent.info_hash)
		var theirhash = UnicodeFormatter.byteArrayToHex(t.getInfoHash())
		debug("ours: "+ourhash)
		debug("theirs: "+theirhash)
		ourhash should equal (theirhash)
	}
	*/
}