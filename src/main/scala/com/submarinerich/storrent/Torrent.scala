package com.submarinerich.storrent

import util.BencodeDecoder 
import util.BencodeEncoder
import java.util.Date
import scala.io.Source
import util.UnicodeFormatter
import grizzled.slf4j.Logger

case class ZeroFileException() extends Exception

object FileMode extends Enumeration
{
  type FileModeType = Value
  val SingleFile,MultiFile,UnknownFileMode = Value
}

class Torrent
{
  import FileMode._
  import Protocol._

	var log = Logger(classOf[Torrent])
  var fileMode : FileModeType = UnknownFileMode
  var creationDate : Date = new Date()
  var announce : Option[String] = None
  var announceList : List[Tracker] = List()
  var createdBy : String = ""
  var encoding : Option[String] = None
  var info_hash : Array[Byte] = Array()
  var info_hash_hex = ""
  var files : List[TorrentFile] = List()
  var name : String = ""
  var pieceLength : Int = 0
  var pieces : Array[TorrentPiece] = Array()
           
  def udptrackers : List[Tracker] = announceList.filter( a => a.protocol == UDP )

  def httptrackers : List[Tracker] = announceList.filter( a => a.protocol == HTTP )

  override def toString : String = {
    "name : "+name+" announce: "+announce.get+" al:"+announceList+" "+createdBy+" pl: "+pieceLength.toString+" pieces: "+pieces.size.toString + "info hash: "+UnicodeFormatter.byteArrayToHex(info_hash)
  }
}

trait Hashable 
{
	def hash( data : Array[Byte] ) : Array[Byte] = {
		import java.security.MessageDigest
		var md  : MessageDigest = MessageDigest.getInstance("SHA-1")
		md.update(data);
		md.digest()
	}
}

object Torrent extends Hashable
{
	var log = Logger("Torrent")

	def fromSource( source : Array[Byte] ) : Torrent = {
		import FileMode._
		import scala.collection.mutable.ListBuffer
    import java.nio.ByteBuffer

    val myObject = BencodeDecoder.decode( source )  
    var contents : Map[String,Any] = myObject.asInstanceOf[Map[String,Any]]    
    var t = new Torrent()
    t.creationDate = new Date( contents("creation date").asInstanceOf[Int] )

    if( contents.contains("announce") )
      t.announce = Some(  contents("announce").toString )

    if( contents.contains("announce-list") ){
      var al : ListBuffer[Tracker] = new ListBuffer[Tracker]()
      contents("announce-list").asInstanceOf[List[List[String]]].map( a => if( a.size > 0 ) al += new Tracker(a(0).toString) )
      t.announceList = al.toList
    }
		
		t.info_hash = hash( BencodeDecoder.infoBytes( source ) ) 
		t.info_hash_hex = UnicodeFormatter.byteArrayToHex( t.info_hash )

    if( contents.contains("created by") )
      t.createdBy = contents("created by").toString

    if( contents.contains("encoding") )
      t.encoding = Some(contents("encoding").toString)
		
    var files : ListBuffer[TorrentFile] = new ListBuffer[TorrentFile]()

    if( contents.contains("info") ){
      var info : Map[String,Any] = contents("info").asInstanceOf[Map[String,Any]]
				
      if( info.contains("name") )
        t.name = info("name").toString

      if( info.contains("piece length") ){
				t.pieceLength = info("piece length").asInstanceOf[Int]
			}

			var p : ListBuffer[TorrentPiece] = new ListBuffer[TorrentPiece]()
			
      if( info.contains("pieces") ){
				//log.info(" pieces: "+info("pieces"))
        var pieces : Array[Byte] = info("pieces").asInstanceOf[Array[Byte]]
				//log.info(" bytes in the pieces bit "+pieces.size)
				var iterator = 0
				var counter = 0

				while(iterator < pieces.size){
					var lb = ByteBuffer.allocate(20)
					
				 	for( i <- 0 until 20 ) 
						lb.put(i,pieces(iterator+i))
						
					p += new TorrentPiece(t.info_hash_hex,counter, lb.array() ,t.pieceLength )
					counter += 1
					iterator += 20
				}
			}
			t.pieces = p.toArray
			log.info(" piece count = "+t.pieces.size)

      if( info.contains("files") ){ //multi file mode
				t.fileMode = MultiFile
				var c = 0
        info("files").asInstanceOf[List[Map[String,Any]]].map( a => {
         var f = new TorrentFile( a("path").asInstanceOf[List[String]](0).toString,c )
         f.length = a("length").asInstanceOf[Int]
         files += f
         c += 1
      })
			}else{ //single file mode
				if(info.contains("name")){
					t.fileMode = SingleFile
					var f = new TorrentFile( info("name").toString,-1 )
					f.length = info("length").asInstanceOf[Int]
					files += f
				}
			}
    }
		if( files.size < 1 )
			throw new ZeroFileException()
		
    t.files = files.toList

    t 
  }

  def sanitize( urlstring : String ) : String = {
    urlstring.replace("[","\\[").replace("]","\\]")
  }
  
  def fromURL( urlstring : String ) : Torrent = {
    import org.apache.http.impl.nio.client.DefaultHttpAsyncClient
    import org.apache.http.nio.client.HttpAsyncClient
    import org.apache.http.client.methods.HttpGet
  	import org.apache.commons.io.IOUtils

    var httpclient = new DefaultHttpAsyncClient()

    httpclient.start()

    var source : Array[Byte] = Array()
    
    try{
    	log.info("attempting request")
    	var parts = urlstring.split("/")
    	var filename = parts( parts.size - 1)
      var urlStart = urlstring.replace(filename,"")
      var fullurl = urlStart + java.net.URLEncoder.encode(filename)
      var request = new HttpGet(fullurl)
      var future = httpclient.execute(request,null)
      var response = future.get()
      if( response.getStatusLine().getStatusCode() == 200 )
        source = IOUtils.toByteArray(response.getEntity().getContent()) 
    }catch{
      case e : Exception => log.warn("problem! : "+e)
      case _ => log.error(" something else!")
    }finally{
      httpclient.shutdown()
    }
    //log.info("byte number: "+source.size)
    fromSource( source ) 
  }

  case class LocalTorrentFileNotFoundException() extends Exception
  
  def fromFile( localFile : String ) : Torrent = {
    import java.io.FileInputStream
    import org.apache.commons.io.IOUtils
    import java.io.File
    var f = new File(localFile)
    if(f.exists){
      var fis = new FileInputStream(f)
      val source = IOUtils.toByteArray(fis)
      fromSource(source)
    }else{
      throw new LocalTorrentFileNotFoundException()
      null
    }
  }

  def fromLocalTest( localurl : String ) : Torrent = {
    fromFile("src/test/resources"+localurl)
  }
}