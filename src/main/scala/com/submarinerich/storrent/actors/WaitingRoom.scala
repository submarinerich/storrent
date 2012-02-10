package com.submarinerich.storrent.actors


import org.jboss.netty.channel.Channel
import com.submarinerich.storrent.Peer
import java.util.Date
import scala.actors.Actor
import grizzled.slf4j.Logger

case class AddPatient( p : Channel, pe : Peer )
case class RemovePatient( p : Channel, pe : Peer )
case class GetSize()

object WaitingRoom extends Actor 
{
 
	var log = Logger("WaitingRoom")
	var lifesupport : LifeSupport = new LifeSupport

  def act(){
    loop{
      react{
        case AddPatient(p : Channel, pe : Peer ) => {
          lifesupport.patients += (p -> pe )
        }
        case RemovePatient( p : Channel, pe: Peer ) => {
          if( lifesupport.patients.contains(p) ){
            Catalogue ! Disconnected(lifesupport.patients(p))
            lifesupport.patients -=  p 
          }
        }
				case GetSize() => {
					reply( lifesupport.patients.size )
				}
        case _ => log.info("unknown message") 
      }
    }
  }
  start
}