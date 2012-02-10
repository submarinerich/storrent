package com.submarinerich.storrent.actors

import java.util.Date
import scala.actors.Actor
import grizzled.slf4j.Logger
import com.twitter.conversions.time._
import org.jboss.netty.channel.Channel
import com.submarinerich.storrent.{Peer,Message}

class LifeSupport extends Actor 
{
  import scala.collection.mutable.HashMap

  var patients : HashMap[Channel,Peer] = new HashMap[Channel,Peer]()
	var log = Logger(classOf[LifeSupport])

  def act(){
    loop{
      Thread.sleep(90.seconds)
      patients.keys.map( a => {
        a.write( Message.keepAlive )
      })
      log.info("sent all keepalive messages, waiting another 90")
    }
  }
  start
}