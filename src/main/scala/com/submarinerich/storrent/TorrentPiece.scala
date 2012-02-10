package com.submarinerich.storrent

import java.nio.ByteBuffer
import util.ByteUtils
import com.twitter.conversions.time._

case class PieceOfPiece( torrentHash : String, index : Int, start : Int, size : Int, data : Option[ByteBuffer])
case class TorrentPiece(torrenthash: String,index : Int, signatureInput : Array[Byte], approximateLength : Long )
extends Hashable
{
	import scala.collection.mutable.ListBuffer
	
  var pieceIndex : Int = index
  var signature : Array[Byte] = signatureInput
  lazy val signatureHex : String = ByteUtils.hexString(signature)
  var torrentSignatureHex : String = torrenthash
  var pieceLength : Long = approximateLength
  private var undownloaded : ListBuffer[PieceOfPiece] = new ListBuffer[PieceOfPiece]()
  private lazy val random = new scala.util.Random( com.twitter.util.Time.now )
  var confirmed : Boolean = false
	var numberNeeded : Int = 1000

  def resetPieces : Unit = {
    var ud = new ListBuffer[PieceOfPiece]()
    var iter = 0
    while( iter < pieceLength.toInt ){
      ud += new PieceOfPiece(torrentSignatureHex,pieceIndex, iter, 16384, None )
      iter += 16384
    }
    undownloaded = ud
		numberNeeded = undownloaded.size
  }
  resetPieces

  def markDownloaded( startOffset : Int, length : Int ) : Unit = {
    var theOnes = undownloaded.filter( a => a.start == startOffset && a.size == length )
    theOnes.map( a => undownloaded -= a )
		numberNeeded = undownloaded.size
  }

	def markAllAsDownloaded : Unit = {
		undownloaded = new ListBuffer[PieceOfPiece]()
		numberNeeded = undownloaded.size
	}

  def randomOutstandingPiece : Option[PieceOfPiece] = {
    if( undownloaded.size > 0 ){
      Some(undownloaded( java.lang.Math.abs(random.nextInt) % undownloaded.size))
    }else{
      None
    }
  }

  def done : Boolean = {
    numberNeeded == 0 || confirmed
  }
  
  def needed : List[PieceOfPiece] = {
    undownloaded.toList
  }

	override def toString = {
		signatureHex + " pieceLength "+pieceLength// +" "+ByteUtils.hexString( bytes.array() )
	}
}