package models

import java.time._
import play.api.libs.json._

case class Thread(threadId: Int, forumId: Int, originalId: Int, title: String, url: String, authorId: Int, originalAuthorId: Int, created: Instant)
case class ThreadData(thread: Thread, postCount: Int, lastPage: Int)
case class Post(postId: Int, originalId: Int, forumId: Int, threadId: Int, authorId: Int, originalAuthorId: Int, author: String, created: Instant, text: String, wordCount: Int)
case class Author(authorId: Int, originalId: Int, name: String)

case class PostStats(post: Post, fog: Float, fleschEase: Float, fleschGrade: Float)
case class WordEntry(forumId: Int, threadId: Int, posterId: Int, postId: Int, word: String, count: Int)