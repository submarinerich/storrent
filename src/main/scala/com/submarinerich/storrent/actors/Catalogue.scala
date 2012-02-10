package com.submarinerich.storrent.actors

import com.submarinerich.storrent.Peer
import java.util.Date
import scala.actors.Actor
import grizzled.slf4j.Logger

case class Input( torrenthash : String, p : Peer, pieceNumber : Int)  
case class Query( torrenthash : String, pieceNumber : Int )
case class PeerQuery( torrenthash : String, p : Peer )
case class Disconnected( p : Peer )
case class ConfirmedPiece( torrentHash : String, pieceNumber : Int )
case class CatalogEntry(torrenthash : String, p : Peer, pieceNumber : Int)
case class HistogramEntry( pieceNumber: Int, entries: Int)

object Catalogue extends Actor
{
  import scala.collection.mutable.ListBuffer
  import scala.collection.mutable.HashMap
  
  var log = Logger( "Catalogue" )
  var histogram : HashMap[Int,Int] = new HashMap[Int,Int]
	var confirmed : ListBuffer[ConfirmedPiece] = new ListBuffer[ConfirmedPiece]()
  var cat : ListBuffer[CatalogEntry] = new ListBuffer[CatalogEntry]()

  def act(){
    loop{
      react{
        case Input( torrenthash : String, p : Peer, pieceNumber : Int ) => {
	
          var c = cat.filter( a => a.torrenthash == torrenthash && a.p == p && a.pieceNumber == pieceNumber )
					var matchingConfirmedPieces = confirmed.filter( a => a.torrentHash == torrenthash && a.pieceNumber == pieceNumber)

          if( c.size == 0  && matchingConfirmedPieces.size == 0 ){
            cat += new CatalogEntry(torrenthash,p,pieceNumber)
            if(histogram.contains(pieceNumber))
              histogram(pieceNumber) += 1
            else
              histogram += (pieceNumber -> 1)
          }
        }
        case PeerQuery( torrenthash : String, p : Peer ) => {
          var c = cat.filter( a => a.p == p && a.torrenthash == torrenthash )
          var piecesUserHas = new ListBuffer[Int]

          c.map( a => piecesUserHas += a.pieceNumber )

          var v = new scala.collection.mutable.ArrayBuffer[HistogramEntry]()
          
          histogram.keys.map( a => v += new HistogramEntry(a, histogram(a)))
          
          scala.util.Sorting.stableSort(v, (e1: HistogramEntry, e2: HistogramEntry) => e1.entries < e2.entries )

          var orderedFeedback = new ListBuffer[Int]
          for( rarePiece <- v )
            if( piecesUserHas.contains( rarePiece.pieceNumber ) )
              orderedFeedback += rarePiece.pieceNumber

          reply(orderedFeedback.toList)
        }
        case Query( torrenthash : String, pieceNumber : Int ) => {
          var c = cat.filter( a => a.torrenthash == torrenthash && a.pieceNumber == pieceNumber ) 
          var p = new ListBuffer[Peer]()
          c.map( a => p += a.p )
          reply( p.toList )
        }
        case Disconnected( p : Peer ) => {
          var c = cat.filter( a => a.p == p )

          c.map( a => {
            histogram(a.pieceNumber) -= 1
            cat -= a 
            })

        }
        case ConfirmedPiece( torrenthash : String, pieceNumber : Int ) => {
          var matchingCataloguePieces = cat.filter( a => a.torrenthash == torrenthash && a.pieceNumber == pieceNumber )
          matchingCataloguePieces.map( a => cat -= a )
          histogram -= pieceNumber
					confirmed += new ConfirmedPiece( torrenthash, pieceNumber )
        }
      }
    }
  }
  start
}