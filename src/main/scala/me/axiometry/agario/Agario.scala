package me.axiometry.agario

import akka.io.IO
import akka.pattern.ask
import akka.actor.ActorSystem
import akka.util.Timeout

import spray.can.Http
import spray.http._
import HttpHeaders._
import HttpMethods._
import MediaTypes._
import spray.client.pipelining._

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{ Try, Failure, Success }

import grizzled.slf4j.Logging


object Agario extends Logging {
  implicit val system = ActorSystem()
  import system.dispatcher

  private[this] implicit val timeout: Timeout = 10 seconds
  private[this] val ServerPattern = "<option value=\"([a-zA-Z]+)\\-([a-zA-Z]+)\">[a-zA-Z ]+</option>".r

  def servers: List[Server] = {
    val submit: HttpRequest => Future[HttpResponse] = sendReceive
    val response: Future[HttpResponse] = submit(Get("http://agar.io/"))

    val page = Try(Await.ready(response, timeout.duration)) match {
      case Success(_) if response.value.get.isSuccess =>
        val res = response.value.get.get
        Some(res.entity.data.asString)
      case Success(_) => response.value.get match {
        case Failure(error) =>
          debug("Request to agar.io failed", error)
          None
        case _ => None
      }
      case Failure(error) =>
        debug("Request to agar.io failed", error)
        None
      case _ => None
    }

    page match {
      case Some(page) =>
        (page split '\n' toList) flatMap {
          case ServerPattern(region, name) => serverAddr(region, name).map(addr => Server(region, name, addr))
          case _ => None
        }
      case None => List()
    }
  }

  private[this] def serverAddr(region: String, name: String): Option[ServerAddress] = {
    debug(s"Sending request about $region-$name")
    val submit: HttpRequest => Future[HttpResponse] = sendReceive
    val response: Future[HttpResponse] = submit(Post("http://m.agar.io/", s"$region-$name"))

    Try(Await.ready(response, timeout.duration)) match {
      case Success(_) if response.value.get.isSuccess =>
        val res = response.value.get.get
        res.entity.data.asString.split('\n')(0) match {
          case ServerAddress(addr) => Some(addr)
          case str =>
            debug(s"Not an address: $str")
            None
        }
      case Success(_) => response.value.get match {
        case Failure(error) =>
          debug("Request to m.agar.io failed", error)
          None
        case _ => None
      }
      case Failure(error) =>
        debug("Request to m.agar.io failed (no response)", error)
        None
      case _ => None
    }
  }
}