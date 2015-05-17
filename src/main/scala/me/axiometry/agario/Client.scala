package me.axiometry.agario

import me.axiometry.agario.net._

import akka.actor.{ ActorSystem, Props }
import grizzled.slf4j.Logging
import spray.can.Http
import spray.can.websocket.frame._
import spray.http._
import spray.httpx.RequestBuilding._

case class Client private (val name: String, val server: Server)(implicit system: ActorSystem) extends Logging {
  private[this] val websocket = {
    val ServerAddress(host, port) = server.address
    val connect = Http.Connect(host, port, false)
    val request = (Get("/") ~> addHeader(HttpHeaders.Host(host, port))
                            ~> addHeader(HttpHeaders.Connection("Upgrade"))
                            ~> addHeader("Upgrade", "websocket")
                            ~> addHeader("Sec-WebSocket-Version", "13")
                            ~> addHeader("Sec-WebSocket-Key", "x3JJHMbDL1EzLkh9GBhXDw==")
                            ~> addHeader("Sec-WebSocket-Extensions", "permessage-deflate"))
    system.actorOf(Props(
      new WebSocketClient(connect, request) {
        def onConnect() {
          debug("Connected")

          connection ! BinaryFrame {
            val view = DataView(5)
            view.setUint8(0, 254)
            view.setUint32(1, 1)(ByteOrder.LittleEndian)
            view.toByteString()
          }
          connection ! BinaryFrame {
            val view = DataView(5)
            view.setUint8(0, 255)
            view.setUint32(1, 1)(ByteOrder.LittleEndian)
            view.toByteString()
          }
        }

        def onMessage(frame: Frame) {

          val view = DataView(frame.payload.toArray)
          debug("Got packet: " + view.getUint8(0))


        }

        def onClose() {
          debug("Disconnected")
        }
      }
    ))
  }


}
