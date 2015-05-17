package me.axiometry.agario.net

import akka.actor.ActorSystem
import akka.io.IO
import spray.can.Http
import spray.can.server.UHttp
import spray.can.websocket._
import spray.can.websocket.frame._
import spray.http.HttpRequest

abstract class WebSocketClient(connect: Http.Connect, val upgradeRequest: HttpRequest)(implicit system: ActorSystem) extends WebSocketClientWorker {
  IO(UHttp) ! connect

  def businessLogic: Receive = {
    case UpgradedToWebSocket =>
      onConnect()

    case frame: Frame =>
      onMessage(frame)

    case _: Http.ConnectionClosed =>
      onClose()
      context.stop(self)
  }

  def onConnect()
  def onMessage(frame: Frame)
  def onClose()
}