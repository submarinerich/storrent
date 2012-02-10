package com.submarinerich.storrent.actors

import com.twitter.ostrich.stats.Stats
import com.twitter.conversions.time._
import com.twitter.util.Time
import com.submarinerich.storrent.{TorrentClient,PieceOfPiece}
import scala.actors.Actor
import grizzled.slf4j.Logger

class LibraryBacklog extends Actor
{
  var items = new scala.collection.mutable.ArrayBuffer[PieceOfPiece]()
  var log = Logger(classOf[LibraryBacklog])

  def act(){
    loop{
      if(items.size > 0)
        Thread.sleep(1.seconds)
      else
        Thread.sleep(10.seconds)
      //log.info("size: "+items.size)
      items.map( a => {
        Librarian ! a
        Thread.sleep(20.millis)
      })
    }
  }
  start
}

object Librarian extends Actor
{
  var log = Logger("Librarian")
  var torrentClient : Option[TorrentClient] = None
  var backlog = new LibraryBacklog

  def act(){
    loop{
      react{
        case tc : TorrentClient => {
          torrentClient = Some(tc)
        }
        case p : PieceOfPiece => {
          torrentClient match {
            case Some( t : TorrentClient ) => {
              Stats.incr("wrote_piece_of_piece")
              Stats.setLabel("pieces_left_to_download",t.popleft.toString)
              try{
                t.writePieceOfPiece(p, p.data.get.array)
                t.torrent.pieces(p.index).markDownloaded( p.start, p.size )
              }catch{
                case nrce : java.nio.channels.NonReadableChannelException => {
                  if(!backlog.items.contains(p))
                    backlog.items += p
                }
                case cce : java.nio.channels.ClosedChannelException => {
                  if(!backlog.items.contains(p))
                    backlog.items += p
                }
                case e : Exception => {
                  log.error("some general exception: "+e)
                }
              }finally{
                if( backlog.items.contains(p) )
                  backlog.items -= p
              } 
            }
            case _ => {}
          }
        }
      }
    }
  }
  start
}