package me.axiometry.agario

import akka.actor.ActorSystem

case class Client(val name: String, val server: Server)(implicit system: ActorSystem) {

}