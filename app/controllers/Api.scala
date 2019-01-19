package controllers

import play.api.Environment
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Configuration
import play.api.libs.json._
import play.api.Logger
import akka.actor._
import akka.pattern.ask
import akka.routing._
import akka.util.Timeout
import javax.inject._
import java.io.File
import scala.concurrent.{ExecutionContext,TimeoutException}
import scala.concurrent.duration._
import scala.concurrent.Future

import services.twoplustwo._

class Api @Inject()(
  env: Environment,
  @Named("scraper-service") val scraper: ActorRef) extends Controller
{

  def fetch(key: String, firstPage: Int, lastPage: Int, update: Int) = Action {
    scraper ! DataRequest("http://forumserver.twoplustwo.com/46/sporting-events/", firstPage, lastPage, update == 1)
    Ok("loading data")
  }
  
  def update(key: String) = Action {
    scraper ! FixMissingDataRequest
    Ok("fixing data")
  }
  
  def updateStats(key: String, forumId: Int) = Action {
    scraper ! StatsUpdateRequest(forumId)
    Ok("updating stats...")
  }
}