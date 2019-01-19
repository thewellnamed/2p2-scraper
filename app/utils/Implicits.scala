package utils

import play.api.db._
import play.api.libs.json._
import anorm._
import anorm.SqlParser._
import anorm.NamedParameter.symbol
import scala.Left
import scala.Right
import java.time.Instant

object Implicits {
  // Map between PostgreSQL jsonb type and native json
  implicit def jsonToString: Column[JsValue] = Column.nonNull1{ (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case pgo: org.postgresql.util.PGobject => Right(Json.parse(pgo.getValue))
      case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" +
          value.asInstanceOf[AnyRef].getClass + " to JsValue for column " + qualified))
    }
  }  
}