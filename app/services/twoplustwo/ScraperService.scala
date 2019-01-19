package services.twoplustwo

import akka.actor._
import akka.contrib.throttle._
import akka.contrib.throttle.Throttler._
import akka.routing.SmallestMailboxPool
import play.api.Logger
import play.api.libs.ws.WSClient
import play.api.libs.json._
import javax.inject._
import scala.concurrent.duration._

import java.time._
import java.time.format.DateTimeFormatter

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.Document
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model.Element

import models._

case class DataRequest(baseUrl: String, firstPage: Int, lastPage: Int, updateOnly: Boolean)
case object FixMissingDataRequest

case class PageRequest(baseUrl: String, page: Int, updateOnly: Boolean)
case class ThreadRequest(parent: ActorRef, thread: Thread, baseUrl: String, page: Int, queueAdditionalPages: Boolean)
case class ThreadData(thread: Thread, page: Int, posts: List[Post])

case class StatsUpdateRequest(forumId: Int)
case class FetchStatsData(forumId: Int, offset: Int)
case class ProcessStatsRequest(posts: List[Post], offset: Int)
case class ProcessStatsResponse(posts: List[PostStats], words: List[WordEntry])

object ScraperService {
  def props = Props[ScraperService]
}

class ScraperService @Inject()(ws: WSClient, dao: ScraperDAO) extends Actor {
  import ScraperService._
  
  val worker = context.actorOf(ScraperWorker.props)
  val throttledWorker = context.actorOf(Props(classOf[TimerBasedThrottler], 1 msgsPer 3.second))
  throttledWorker ! SetTarget(Some(worker))
  
  val throttledStats = context.actorOf(Props(classOf[TimerBasedThrottler], 1 msgsPer 30.second))
  throttledStats ! SetTarget(Some(self))
  
  val throttledSelf = context.actorOf(Props(classOf[TimerBasedThrottler], 1 msgsPer 180.second))
  throttledSelf ! SetTarget(Some(self))
  
  def receive = {
    case DataRequest(baseUrl: String, firstPage: Int, lastPage: Int, updating: Boolean) => {
      val url = firstPage match {
        case 1 => baseUrl + "?daysprune=365"
        case _ => baseUrl + "index" + firstPage + ".html?daysprune=365"
      }
      
      Logger.info("fetching: " + url)
      
      val browser = JsoupBrowser()
      val doc = browser.get(url)

      processPage(doc, baseUrl, updating)
      
      if (lastPage > firstPage) {
        (firstPage + 1 to lastPage).foreach { page => throttledSelf ! PageRequest(baseUrl, page, updating) }
      }
    }
    
    case FixMissingDataRequest => {
      val threads = dao.getThreadsToFix()
      
      threads.foreach { t =>
        throttledWorker ! ThreadRequest(self, t, t.url, 1, queueAdditionalPages = true)
      }
    }
    
    case PageRequest(baseUrl: String, page: Int, updating: Boolean) => {
      val url = baseUrl + "index" + page + ".html?daysprune=365";
      Logger.info("fetching: " + url)
      
      val browser = JsoupBrowser()
      val doc = browser.get(url)
      processPage(doc, baseUrl, updating)
    }
    
    case data: ThreadData => dao.addPosts(data.thread, data.page, data.posts)
    
    case req: StatsUpdateRequest => {
      val totalUpdateCount = dao.getCountOfPostsWithoutStats(req.forumId)
      val pages = if (totalUpdateCount % 1000 != 0) { (totalUpdateCount/1000 + 1) } else { totalUpdateCount/1000 }
      
      Logger.info("processing stats: " + totalUpdateCount + " posts...")
      
      (1 to pages).foreach { page => 
        val offset = (page - 1) * 1000
        throttledStats ! FetchStatsData(req.forumId, offset)
      }
    }
    
    case FetchStatsData(forumId: Int, offset: Int) => {
        val posts = dao.getPostsForStatsUpdate(forumId, 1000)
        throttledWorker ! ProcessStatsRequest(posts, offset)
    }
    
    case data: ProcessStatsResponse => dao.updateStats(data.posts, data.words)
  }
  
  def processPage(doc: Document, baseUrl: String, updating: Boolean = false) = {
    val forumIdPattern = "http://forumserver.twoplustwo.com/([0-9]+)/.*".r
    val authorIdPattern = ".*http://forumserver.twoplustwo.com/members/([0-9]+)/.*".r
    
    val forumIdPattern(forumId) = baseUrl
    val topics = doc >?> elementList("#threadslist > #threadbits_forum_" + forumId + " > tr")
    
    if (topics.isDefined) {
      topics.get.foreach { topic =>  
        val fullUrl = topic >> attr("href")("td:nth-child(3) > div > a")
        val url = fullUrl.substring(0, fullUrl.lastIndexOf("/") + 1)
        val title = topic >> text("td:nth-child(3) > div > a")
        val origThreadId = url.substring(url.lastIndexOf("-") + 1, url.lastIndexOf("/")).toInt

        val authorElem = topic >?> element("td:nth-child(3) > div.smallfont > span")     
        val author = authorElem match {
          case Some(e) => e.text
          case None => "guest"
        }
        
        val authorUrl = authorElem match {
          case Some(e) => {
            e.hasAttr("onclick") match {
              case true => e.attr("onclick")
              case false => ""
            }
          }
          
          case None => ""
        }
        
        val origAuthorId = authorUrl match {
          case authorIdPattern(id) => id.toInt
          case _ => 0
        }
        
        val startingPage = updating match {
          case false => 1
          case true => dao.getLastPageForThread(origThreadId.toInt)
        }

        val authorId = dao.getAuthorId(origAuthorId, author)
        
        val threadData = Thread(
            threadId = 0,
            forumId = forumId.toInt,
            originalId = origThreadId.toInt,
            title = title,
            url = url, 
            authorId = authorId,
            originalAuthorId = origAuthorId.toInt,
            created = null)
                       
        val thread = dao.addThread(threadData)
        
        if (thread.isDefined) {
          throttledWorker ! ThreadRequest(self, thread.get, url, startingPage, queueAdditionalPages = true)
        } else {
          Logger.warn("Unable to save thread: " + threadData.title)
        }
      }
    }
  }
}