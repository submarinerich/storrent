package com.submarinerich.storrent

import scala.util.Random
import java.util.Date
import grizzled.slf4j.Logger
import java.io.File
import org.apache.commons.io.FileUtils
import com.twitter.conversions.time._
import com.twitter.util.Time
import com.twitter.ostrich.stats.Stats
import scala.collection.mutable.ListBuffer
import handlers._
import actors._

case class ContainingFolderDoesNotExistException() extends Exception
case class DestinationFileNotFoundException() extends Exception

case class TorrentClient( t : Torrent)(implicit downloadTo : String = "/tmp" ) extends Hashable
{

  import java.nio.ByteBuffer
  import java.io.RandomAccessFile
  import scala.collection.mutable.ListBuffer
  import util.ByteUtils
  import org.jboss.netty.channel.socket.nio.{NioClientSocketChannelFactory,NioDatagramChannelFactory}
  import org.jboss.netty.channel.Channel
  import java.util.concurrent.Executors
  import org.jboss.netty.bootstrap.ClientBootstrap
  import AnnounceEvent._

  private var log = Logger( classOf[TorrentClient] )
	private var seed = new Date()
  private lazy val random = new Random( Time.now )
	var torrent : Torrent = t
	
  def containingFolder : File = {
    var f = new File( downloadTo )
    if( ! f.isDirectory() ){
      log.error(" file : "+downloadTo+" doesn't exist")
      throw new ContainingFolderDoesNotExistException()
    }
    f
  }

  private def unfinishedFile : File = new File( containingFolder + "/"+ torrent.info_hash_hex+".part" )
      
  def initializeFiles( implicit deleteExisting : Boolean = true ) : Unit = {
    if(deleteExisting){
      FileUtils.deleteQuietly(unfinishedFile)

      for( f <- torrent.files )
        FileUtils.deleteQuietly( new File( containingFolder + "/" + f.path) )

      var b = ByteBuffer.allocate( torrent.pieceLength )
      b.clear 
      var fillUp : Byte = 0x00
      for( a <- 0 until torrent.pieceLength )
        b.put(fillUp)
      
      b.flip
      
      var fileChannel = new RandomAccessFile( unfinishedFile,"rw" ).getChannel
      for( a <- 0 until torrent.pieces.size ){
        fileChannel.write(b)
        b.flip
      }
      fileChannel.close
    }else if( unfinishedFile.exists ){
      for( p <- torrent.pieces ){
        if( checkTorrentPiece(p) ){
					p.confirmed = true
					p.markAllAsDownloaded
				}
      }
    }else{
			log.info("unfinished file doesn't exist")
      initializeFiles(true)
    }
    
  }

  def splitUpAndRenameFiles : Unit = {
    var filePointer : Long = 0
    var i : Long = 0
    var inFileChannel = new RandomAccessFile(unfinishedFile,"r").getChannel
    for( file <- torrent.files ){

      var buffer = java.nio.ByteBuffer.allocate(torrent.pieceLength)
      var outFileChannel = new RandomAccessFile(new File( containingFolder + "/" + file.path ),"rw").getChannel

      outFileChannel.position(0)

      while( i < filePointer + file.length ){
        buffer.clear
        inFileChannel.position(i)
        inFileChannel.read(buffer)
        buffer.flip
        outFileChannel.write(buffer)
        i += torrent.pieceLength
      }
      outFileChannel.close

      filePointer += file.length
    }
    inFileChannel.close
    FileUtils.deleteQuietly(unfinishedFile)
  }

  var unwritten : ListBuffer[TorrentPiece] = {
  	var uw = new ListBuffer[TorrentPiece]()
    t.pieces.map( a => uw += a )
    uw
  }

  def popleft : Int = {
    var left = 0
    for( i <- torrent.pieces)
      if( i.confirmed == false )
        left += i.numberNeeded

    left                      
  }

  //returns if the given torrent piece is valid or not
  def checkTorrentPiece( tp : TorrentPiece ) : Boolean = {
    var fc1 = new RandomAccessFile( unfinishedFile,"r" ).getChannel
    var startReadingAt : Long = (tp.pieceIndex*torrent.pieceLength)

    if( fc1.size == 0 || startReadingAt > fc1.size ){
        log.info("file size is innappropriate "+fc1.size)
        System.exit(1)
    }
    
    var buffer = java.nio.ByteBuffer.allocate(torrent.pieceLength)
    
    fc1.position(startReadingAt)
    
    buffer.clear
    
    try{
      var amtRead = fc1.read(buffer)
    }catch{
      case e : Exception => log.error(unfinishedFile+"tried to read starting at "+startReadingAt+" and the file is "+fc1.size)
      case _ => log.error("worse error")
    }
    
    fc1.close
    
    var maybeSignature = hash(buffer.array)
    var accurate = true
    
    for( a <- 0 until tp.signature.length )
      if( maybeSignature(a) != tp.signature(a) )
        accurate = false

    accurate
  }

  def writePieceOfPiece( p : PieceOfPiece, data : Array[Byte] ) : Unit = {

    var tp = torrent.pieces(p.index)

    if( unfinishedFile.exists ){
      if( tp.confirmed == false ){
        var raf = new RandomAccessFile( unfinishedFile,"rw")
        var startWritingAt = (tp.pieceIndex * torrent.pieceLength) + p.start
        raf.seek(startWritingAt)
        raf.write(data)
        raf.close

        if( tp.numberNeeded == 0 ){
          var torrentValid = checkTorrentPiece(tp)

          if(  torrentValid ){
            tp.confirmed = true
            Catalogue ! new ConfirmedPiece(torrent.info_hash_hex,tp.pieceIndex)
          }else
            tp.resetPieces
        }
      }
    }else{
      throw new DestinationFileNotFoundException()
    }
  }

  var trackerDatagramChannelFactory = new NioDatagramChannelFactory( Executors.newCachedThreadPool )

  var trackerBootstrap = new ClientBootstrap(trackerDatagramChannelFactory)

  def announce( e : Event ) : Unit = {
    import org.jboss.netty.channel.{ChannelFuture,ChannelFutureListener}
    import org.jboss.netty.buffer.ChannelBuffer

    trackerBootstrap.setOption("broadcast",true) 
    trackerBootstrap.setOption("sendBufferSize", 65536)
    trackerBootstrap.setOption("receiveBufferSize", 65536)
    trackerBootstrap.setOption("keepAlive",true)
    trackerBootstrap.setOption("tcpNoDelay",true)
    
    torrent.udptrackers.map( tracker => {
      trackerBootstrap.setPipelineFactory( new TrackerUDPHandlerPipelineFactory(torrent,tracker) )
      var channelFuture = trackerBootstrap.connect(tracker.socketAddress)
      channelFuture.addListener( new ChannelFutureListener(){
        def operationComplete( future : ChannelFuture ){
          var announce : Tuple3[ChannelBuffer,Int,Int] = tracker.announceRequestBuffer(torrent,TorrentClient.peer_id,6969)(e)
          if( future.isSuccess )
            future.getChannel.write(announce._1)
        }
      })
    })

  }

  var peers = new ListBuffer[Peer]()
  var channels = new ListBuffer[Channel]()
  var peerSocketChannelFactory = new NioClientSocketChannelFactory( Executors.newCachedThreadPool, Executors.newCachedThreadPool ) 
  var peerBootstrap = new ClientBootstrap(peerSocketChannelFactory)

  def connectToPeers : Unit = {
    import org.jboss.netty.channel.{Channel,ChannelFuture,ChannelFutureListener}
    import java.net.InetSocketAddress

    var lb = new ListBuffer[Channel]()
    peerBootstrap.setOption("broadcast",true) 
    peerBootstrap.setOption("sendBufferSize", 65536)
    peerBootstrap.setOption("receiveBufferSize", 180224)
    peerBootstrap.setOption("keepAlive",true)
    peerBootstrap.setOption("tcpNoDelay",true)

    PeerWrangler !? PeerListRequest() match {
     case pl : List[Peer] => pl.map( a => peers += a )
     case _ => {}
    }
    for( p <- peers){
      if( ! p.connected ){
        p.connected = true
        peerBootstrap.setPipelineFactory( new PeerwireHandlerPipelineFactory(p,torrent) ) 
      var connectionFuture: ChannelFuture = peerBootstrap.connect(new InetSocketAddress(p.address,p.port) )
        connectionFuture.addListener( new ChannelFutureListener(){
           def operationComplete( future : ChannelFuture ){
              if( future.isSuccess ){
                var writeFuture = future.getChannel.write(p.makeHandshake)
                writeFuture.addListener( new ChannelFutureListener(){ 
                  def operationComplete( future1 : ChannelFuture ){
                    channels += future1.getChannel
                  }
                })
              }
           }
         })
      }
    }
  }

  def askForOneRemaining : Unit = {
      var incomplete = torrent.pieces.filter( a => a.numberNeeded != 0 )
      if( incomplete.size > 0 ){
        var randomPop = incomplete(0).randomOutstandingPiece
        randomPop match {
          case Some( pop : PieceOfPiece ) => {
            channels.map( a => a.write( Message.requestMultiple( List(pop) ) ) )
          }
          case _ => {}
        }
      }
      Thread.sleep(500.millis)
  }

  def cleanupResources : Unit = {
    peerBootstrap.releaseExternalResources
    peerSocketChannelFactory.releaseExternalResources
    trackerDatagramChannelFactory.releaseExternalResources
    trackerBootstrap.releaseExternalResources
  }

  def download( updateFunction: Double => Unit  ) : Unit  = {
    import AnnounceEvent._
    import scala.util.control._

    var totalPop : Int = 0
		torrent.pieces.map(a => totalPop += (torrent.pieceLength / 16384))

    var finishedPieces = torrent.pieces.filter( a => a.confirmed == true )

		finishedPieces.map( a => {
			Catalogue ! new ConfirmedPiece(torrent.info_hash_hex,a.pieceIndex)
			a.markAllAsDownloaded 
		})

		var unfinishedPieces = torrent.pieces.filter( a => a.confirmed == false )
		
		unfinishedPieces.map( a => a.resetPieces )
		
		Librarian ! this

    announce(StartedEvent)

    Thread.sleep(3.seconds)
    
    connectToPeers
    
    var firstReading = Time.now
    var amountDownloaded = totalPop - popleft
    
    Breaks.breakable{
      while( finishedPieces.size < torrent.pieces.size ){
        Thread.sleep(10.seconds)
               
        updateFunction((1.0f - ((popleft*100.0f)/(totalPop*100.0f))) *100.0f)
        
        if( popleft < (totalPop*.2) ){
          for( i <- 0 until 6)
            askForOneRemaining                  
        }

        if( Time.now - firstReading > 7.minutes ){
          announce(NoneEvent)
          Thread.sleep(3.seconds)
          connectToPeers
          firstReading = Time.now
        }

        finishedPieces = torrent.pieces.filter( a => a.confirmed == true )
        var piecesLeft = torrent.pieces.size - finishedPieces.size
        if( piecesLeft == 1 || popleft < 30 ){
          var lastPiece = torrent.pieces.filter(a => a.confirmed == false )(0)
          var validLastPiece =checkTorrentPiece(lastPiece)
          if( validLastPiece ){
            lastPiece.confirmed = true
            finishedPieces = torrent.pieces.filter( a => a.confirmed == true )
            Breaks.break
          }
        }
      }
    }
    announce(StopEvent)

    splitUpAndRenameFiles
  }
}

object TorrentClient
{   
  import scala.actors.Futures._
  import com.twitter.ostrich.admin.RuntimeEnvironment
  import com.twitter.ostrich.admin.AdminHttpService
  import com.twitter.ostrich.stats.Stats

  private lazy val random = new Random( Time.now )
  private var my_peer_id : String = ""
  
  def peer_id : String = {
    if( my_peer_id.length == 0 )
        my_peer_id = "-SR0001-%12d".format((java.lang.Math.abs( random.nextLong())) % 1000000000000L)
    my_peer_id
  }

  val log = Logger("TorrentClient")

  def displayUpdate( f : Double ) : Unit = {
    log.info("percentage downloaded: "+"%03.02f".format(f)+"% (of 100%) ")
  }

  def main(args: Array[String]): Unit = {
    var service: AdminHttpService = new AdminHttpService(9000, 20, Stats, new RuntimeEnvironment(getClass))
    service.start()
    var i = 0
    var requiredFilesFound = false

    for( a <- args ){
      //Console.println(" argument "+i+" "+a.toString)
      if(a.contains(".torrent"))
        requiredFilesFound = true
      i += 1
    }

    if (args.size < 2 || !requiredFilesFound ) {
      Console.println("usage: storrent <torrent> [directory]")
      Console.println(" where <torrent> can be either an http address or a local file, and [directory] is a local directory")
      System.exit(1)
    }

    var torrentSuggestion = ""
    var downloadingTo = ""

    if( args(0).contains(".torrent") ){
      torrentSuggestion = args(0)
      downloadingTo = args(1).toString
    }

    if( args(1).contains(".torrent") ){
      torrentSuggestion = args(1)
      downloadingTo = args(2).toString
    }

    if( torrentSuggestion.length == 0 ){
      log.error("error finding torrent file")
      System.exit(1)
    }

    if( !(new File(downloadingTo)).exists || !(new File(downloadingTo)).isDirectory ){
      log.error("couldn't find download location")
      System.exit(1)
    }

    var t : Option[Torrent] = None

    if( torrentSuggestion.contains("http://") )
      t = Some(Torrent.fromURL(torrentSuggestion))
    else
      t = Some(Torrent.fromFile(torrentSuggestion))
    
    Stats.setLabel("downloading",torrentSuggestion)
    Stats.setLabel("downloadingTo",downloadingTo)
    Stats.setLabel("pieces in torrent ",t.get.pieces.size.toString)

    var tc = new TorrentClient(t.get)(downloadingTo)
		tc.initializeFiles(false)
    tc.download(displayUpdate)
    log.info("cleaning up after myself")

    var a = future{
      tc.cleanupResources
    }

    log.info("exiting")
    System.exit(1)
    a.apply()
  }
}