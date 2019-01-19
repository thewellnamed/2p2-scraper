package services.twoplustwo

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Logger
import play.api.Environment
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import scala.concurrent.duration._
import scala.concurrent.Future
import javax.inject._
import java.time.Instant
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import models._

class ScraperDAO @Inject()(db: Database, env: Environment)
{
  val postParser = {
    get[Int]("post_id") ~
    get[Int]("forum_id") ~
    get[Int]("thread_id") ~
    get[Int]("poster_id") ~
    get[String]("content") map {
      case postId ~ forumId ~ threadId ~posterId ~ content => 
        Post(postId, 0, forumId, threadId, posterId, 0, "", null, content, 0)
    }
  }
  
  val threadParser = {
    get[Int]("thread_id") ~
    get[Int]("orig_id") ~
    get[Int]("forum_id") ~
    get[Int]("poster_id") ~
    get[String]("title") ~
    get[String]("url") ~
    get[Instant]("created") map {
      case threadId ~ origId ~ forumId ~ posterId ~ title ~ url ~ created =>
        Thread(threadId, forumId, origId, title, url, posterId, 0, created)
    }
  }
  
  val posters: scala.collection.mutable.Map[Int, Int] = new scala.collection.mutable.HashMap[Int, Int]()
  posters(0) = 1 // guest
  
  def getAuthorId(originalId: Int, name: String): Int = {
    db.withConnection { implicit c =>
      
      if (posters.contains(originalId)) {
        return posters(originalId)
      }
      
      val existing = SQL("SELECT poster_id FROM posters WHERE orig_id={originalId}")
          .on('originalId -> originalId)
          .as(SqlParser.int("poster_id").singleOpt)
          
      if (existing.isDefined) {
        posters(originalId) = existing.get
        return existing.get
      } else {
        val id = SQL("INSERT INTO posters (name, orig_id) VALUES ({name}, {originalId})")
           .on('name -> name, 'originalId -> originalId)
           .executeInsert()
           
        id match {
          case Some(l) => {
            posters(originalId) = l.toInt
            return l.toInt
          }
          
          case None => return 0
        }
      }
    }
  }
  
  def addThread(t: Thread): Option[Thread] = {
    db.withConnection { implicit c =>
      val existing = SQL("SELECT thread_id FROM threads WHERE orig_id={originalId}")
          .on('originalId -> t.originalId)
          .as(SqlParser.int("thread_id").singleOpt)
      
      existing match {
        case Some(id) => Some(Thread(id, t.forumId, t.originalId, t.title, t.url, t.authorId, t.originalAuthorId, t.created))
        case None => {
          val id = SQL("""INSERT INTO threads (orig_id, forum_id, poster_id, title, url, created) 
                            VALUES ({originalId}, {forumId}, {posterId}, {title}, {url}, {created})""")
           .on('originalId -> t.originalId, 
               'forumId -> t.forumId,
               'posterId -> t.authorId,
               'title -> t.title,
               'url -> t.url,
               'created -> t.created)
           .executeInsert()
          
          id match {
            case Some(l) => Some(Thread(l.toInt, t.forumId, t.originalId, t.title, t.url, t.authorId, t.originalAuthorId, t.created))
            case None => None
          }          
        }
      }  
    }
  }
  
  def getThreadsToFix(): List[Thread] = {
    db.withConnection { implicit c =>
      SQL("""SELECT t.thread_id, t.orig_id, t.forum_id, t.poster_id, t.title, t.url, t.created 
             FROM threads t WHERE (SELECT max(p.seq) - COUNT(p.comment_id) FROM posts p WHERE p.thread_id=t.thread_id) != 0""")
         .as(threadParser.*)
    }
  }
  
  def addPosts(thread: Thread, page: Int, posts: List[Post]) = {
    db.withConnection { implicit c =>
      var seq = ((page - 1) * 25)  + 1;
     
      posts.foreach { p =>
        val authorId = getAuthorId(p.originalAuthorId, p.author)
        SQL("DELETE FROM posts WHERE comment_id={originalId} OR (thread_id={threadId} AND seq={seq})")
            .on('originalId -> p.originalId,
                'threadId -> thread.threadId,
                'seq -> seq)
            .executeUpdate()
            
        SQL("""INSERT INTO posts (thread_id, forum_id, poster_id, comment_id, seq, created, content, word_count, has_stats) 
               VALUES ({threadId}, {forumId}, {authorId}, {commentId}, {seq}, {created}, {content}, {count}, 0)""")
           .on('threadId -> thread.threadId,
               'forumId -> thread.forumId,
               'authorId -> authorId,
               'commentId -> p.originalId,
               'seq -> seq,
               'created -> p.created,
               'content -> p.text,
               'count -> p.wordCount)
           .executeInsert(SqlParser.scalar[Int].single)
           
        if (seq == 1) {
          SQL("UPDATE threads SET created={created} WHERE thread_id={threadId}")
              .on('created -> p.created,
                  'threadId -> thread.threadId)
              .executeUpdate()
        }
      
        seq += 1
      }
    }
  }
  
  def getLastPageForThread(originalThreadId: Int): Int = {
    db.withConnection { implicit c =>
      val lastPost = SQL("""SELECT count(p.comment_id) AS posts 
                            FROM posts p JOIN threads t ON (t.thread_id=p.thread_id)
                            WHERE t.orig_id={originalId}""")
          .on('originalId -> originalThreadId)
          .as(SqlParser.int("posts").singleOpt)
          
      lastPost match {
        case Some(posts) => posts/25 + 1
        case None => 1
      }
    }
  }
  
  def getCountOfPostsWithoutStats(forumId: Int): Int = {
    db.withConnection { implicit c=>
      val total = SQL("SELECT count(post_id) AS posts FROM posts WHERE forum_id={forumId} AND created > '2016-09-01' AND has_stats=0")
          .on('forumId -> forumId)
          .as(SqlParser.int("posts").single)
          
      total
    }
  }
  
  def getPostsForStatsUpdate(forumId: Int, limit: Int): List[Post] = {
    db.withConnection { implicit c =>
      Logger.info("fetch next stats batch")
      
      SQL("SELECT post_id, forum_id, thread_id, poster_id, content FROM posts WHERE forum_id={forumId} AND created > '2016-09-01' AND has_stats=0 ORDER BY post_id LIMIT {limit}")
        .on('forumId -> forumId, 'limit -> limit)
        .as(postParser.*)
    }
  }
  
  def updateStats(posts: List[PostStats], words: List[WordEntry]) = {
    db.withConnection { implicit c =>
      // update post stats
      Logger.info("-----> DAO update " + posts.length + " posts")
      
      posts.foreach { p =>
        val fog = if (p.fog.isNaN || p.fog.isInfinite) { None } else { Some(p.fog) }
        val ease = if (p.fleschEase.isNaN || p.fleschEase.isInfinite) { None } else { Some(p.fleschEase) }
        val grade = if (p.fleschGrade.isNaN || p.fleschGrade.isInfinite) { None } else { Some(p.fleschGrade) }
        
        val res = SQL("UPDATE posts SET has_stats=1, fog_index={fog}, flesch_ease={ease}, flesch_grade={grade} WHERE post_id={postId}")
            .on('postId -> p.post.postId, 
                'fog -> fog,
                'ease -> ease,
                'grade -> grade)
            .executeUpdate()
            
        if (res != 1) { Logger.info("result: " + res) }
      }
      
      // process words
      val forums = words.map { w => w.forumId } distinct
      val threads = words.map { w => w.threadId } distinct
      val posters = words.map { w => w.posterId } distinct
      
      forums.foreach { f =>
        threads.foreach { t =>
          posters.foreach { p =>
            val wordList = words.iterator
               .filter(w => w.forumId == f)
               .filter(w => w.threadId == t)
               .filter(w => w.posterId == p)
               .toList
            
            val uniqueWords = wordList.map { we => we.word } distinct
            
            uniqueWords.foreach { w =>
              val count = wordList.filter(we => we.word.equals(w)).foldLeft(0)((count, we) => count + we.count)
              val posts = (wordList.filter(we => we.word.equals(w)).map { we => we.postId } distinct).length
              
              val existing = SQL("""SELECT count(*) as count 
                                    FROM words 
                                    WHERE forum_id={forumId} AND thread_id={threadId} AND poster_id={posterId} AND word={word}""")
                .on('forumId -> f, 'threadId -> t, 'posterId -> p, 'word -> w)
                .as(SqlParser.int("count").single)
                
              if (existing > 0) {
                SQL("""UPDATE words 
                       SET count = count + {count}, posts = posts + {posts}
                       WHERE forum_id={forumId} AND thread_id={threadId} AND poster_id={posterId} AND word={word}""")
                  .on('forumId -> f, 
                      'threadId -> t, 
                      'posterId -> p, 
                      'word -> w, 
                      'count -> count,
                      'posts -> posts)
                  .executeUpdate()
              } else {
                SQL("""INSERT INTO words (forum_id, thread_id, poster_id, word, count, posts)
                       VALUES ({forumId}, {threadId}, {posterId}, {word}, {count}, {posts})""")
                  .on('forumId -> f, 
                      'threadId -> t, 
                      'posterId -> p, 
                      'word -> w, 
                      'count -> count,
                      'posts -> posts)
                  .executeInsert(SqlParser.scalar[Int].single)
              }
            }
          }
        }
      }
      
      Logger.info("-----> finished stats update for chunk...")
    }
  }
}