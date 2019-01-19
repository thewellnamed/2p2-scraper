package services.twoplustwo

import akka.actor._
import akka.contrib.throttle._
import akka.contrib.throttle.Throttler._
import play.api.Logger
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import java.net.URL
import java.time._
import java.time.format.DateTimeFormatter
import java.time.Period
import java.util.Locale
import javax.inject._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.collection.JavaConversions._

import models._
import models.fathom._
import models.fathom.Fathom.Stats

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.model.Document

object ScraperWorker {
  def props = Props[ScraperWorker]
}

class ScraperWorker extends Actor {
  import ScraperWorker._
  
  val throttledSelf = context.actorOf(Props(classOf[TimerBasedThrottler], 1 msgsPer 2.second))
  throttledSelf ! SetTarget(Some(self))
  
  def receive = {
    case req: ThreadRequest => {
      val authorIdPattern = ".*http://forumserver.twoplustwo.com/members/([0-9]+)/.*".r
      val commentIdPattern = "postcount([0-9]+)".r
      val commentBrowser = JsoupBrowser.typed()
      
      val url = req.page match {
        case 1 => req.baseUrl
        case _ => req.baseUrl + "index" + req.page + ".html"
      }
      
      Logger.info("fetching: " + url)
      
      val browser = JsoupBrowser()
      val doc = browser.get(url)
      
      val pageText = doc >?> text(".pagenav .vbmenu_control")
      val pagesPattern = ("Page " + req.page + " of ([0-9]+).*").r
      
      val lastPage = pageText match {
        case Some(pt) => {
          pt match {
            case pagesPattern(pageNum) => pageNum.toInt
            case _ => 1
          }
        }
        
        case None => 1
      }
      
      val postElems = doc >> elementList("#posts > div:not(#lastpost)")
      
      val posts = postElems.map { p =>
        val commentHtml = (p >> element(".postbitlinks")).outerHtml
        val commentDoc = commentBrowser.parseString(commentHtml)
        val commentElem = commentDoc >> pElement(".postbitlinks")
        commentElem.underlying.children.select("div").remove()
        val comment = commentElem.text

        val author = p >?> text(".bigusername")
        val originalAuthorLink = p >?> attr("href")(".bigusername")
        val commentIdLink = p >> attr("id")("table > tbody > tr:nth-child(1) > td:nth-child(2) a")
        val posted = p >> text("table > tbody > tr:nth-child(1) > td:nth-child(1)")
        
        var postedDate: Instant = null
        
        if (posted.contains("Today")) {
          val time = LocalTime.parse(posted.substring(7), DateTimeFormatter.ofPattern("hh:mm a")).atOffset(ZoneOffset.of("-05:00"))
          postedDate = LocalDate.now().atTime(time).toInstant
        } else if (posted.contains("Yesterday")) {
          val time = LocalTime.parse(posted.substring(11), DateTimeFormatter.ofPattern("hh:mm a")).atOffset(ZoneOffset.of("-05:00"))
          postedDate = LocalDate.now.minus(Period.ofDays(1)).atTime(time).toInstant
        } else {
          try {
              postedDate = LocalDateTime.parse(posted, DateTimeFormatter.ofPattern("MM-dd-yyyy, hh:mm a")).atOffset(ZoneOffset.of("-05:00")).toInstant
          }
          catch {
            case e: Exception => Unit
          }
        }
        
        val originalAuthorId = originalAuthorLink.getOrElse("") match {
          case authorIdPattern(id) => id.toInt
          case _ => 0
        }
        
        val commentId = commentIdLink match {
          case commentIdPattern(id) => id.toInt
          case _ => 0
        }
        
        val words = comment.split("\\s+")

        Post(postId = 0, 
             originalId = commentId, 
             forumId = req.thread.forumId,
             threadId = req.thread.threadId,
             authorId = 0,
             originalAuthorId = originalAuthorId,
             author = author.getOrElse("guest"),
             created = postedDate,
             text = comment,
             wordCount = words.size)
      }
      
      if (req.queueAdditionalPages && lastPage > req.page) {
        (req.page + 1 to lastPage).foreach { page => 
          throttledSelf ! ThreadRequest(req.parent, req.thread, req.baseUrl, page, queueAdditionalPages = false) 
        }
      }
           
      req.parent ! ThreadData(req.thread, req.page, posts)  
    }
    
    case req: ProcessStatsRequest => {
      val words: scala.collection.mutable.ListBuffer[WordEntry] = new scala.collection.mutable.ListBuffer[WordEntry]()
      val stats: scala.collection.mutable.ListBuffer[PostStats] = new scala.collection.mutable.ListBuffer[PostStats]()
      
      Logger.info("process stats: " + req.offset + " (" + req.posts.length + " posts)")
      
      req.posts foreach { post =>
        val s = Fathom.analyze(post.text)
        stats.append(PostStats(post, Readability.calcFog(s), Readability.calcFlesch(s), Readability.calcKincaid(s)))
        
        s.getUniqueWords() foreach { case (w, count) =>
          val word = w.toLowerCase()
          
          if (word.length > 1 && word.length < 21 && !PorterStemmer.stopWords.contains(word)) {
            val stem = PorterStemmer.stem(word)
            words.append(WordEntry(post.forumId, post.threadId, post.authorId, post.postId, stem, count))
          }
        }
      }
      
      Logger.info("response: " + stats.length)
      sender ! ProcessStatsResponse(stats.toList, words.toList)
    }
  }
}