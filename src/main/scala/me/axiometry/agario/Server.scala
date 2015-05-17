package me.axiometry.agario

import akka.actor.ActorSystem

case class Server(val region: String, val name: String, val address: String)(implicit system: ActorSystem) {
  def createClient(name: String = null): Client = ???
}
