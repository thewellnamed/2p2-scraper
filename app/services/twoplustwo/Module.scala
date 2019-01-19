package services.twoplustwo

import play.libs.akka.AkkaGuiceSupport
import com.google.inject.AbstractModule

class Module extends AbstractModule with AkkaGuiceSupport {
  def configure() = {
    bindActor(classOf[ScraperService], "scraper-service")
    bind(classOf[ScraperDAO]).asEagerSingleton()
  }
}