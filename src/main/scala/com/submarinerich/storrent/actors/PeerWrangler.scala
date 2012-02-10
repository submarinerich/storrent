package com.submarinerich.storrent.actors

import scala.actors.Actor
import grizzled.slf4j.Logger
import com.submarinerich.storrent.Peer

case class PeerListRequest()
case class RemovePeer( p : Peer )

object PeerWrangler extends Actor
{
  var log = Logger("PeerWrangler")
  var peers = new scala.collection.mutable.ListBuffer[Peer]()

  def act(){
    loop{
      react{
        case tc : Peer => {
          var thisPeer = peers.filter( a => a.host == tc.host && a.port == tc.port )
          if( thisPeer.size == 0 )
            peers += tc
        }
        case plc : PeerListRequest => {
        	reply(peers.toList)
        }
        case rp : RemovePeer => {
        	if( peers.contains(rp.p))
        		peers -= rp.p
        }
      }
    }
  }
  start
}