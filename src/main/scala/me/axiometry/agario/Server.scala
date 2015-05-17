package me.axiometry.agario

import akka.actor.ActorSystem
import scala.util._

case class ServerAddress(host: String, port: Int)

object ServerAddress {
  private[this] val ServerPattern = "([^:]+):([0-9]+)".r
  def unapply(string: String): Option[ServerAddress] = string match {
    case ServerPattern(host, port) => Try(port.toInt) match {
      case Success(port) => Some(ServerAddress(host, port))
      case Failure(_) => None
    }
    case _ => None
  }
}
case class Server(val region: String, val name: String, val address: ServerAddress)(implicit system: ActorSystem) {
  def createClient(name: String = null): Client = ???
}
