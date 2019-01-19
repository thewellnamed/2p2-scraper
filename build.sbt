name := """2p2-Scraper"""

version := "1.0.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala, SbtWeb)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  filters,
  specs2 % Test,
  "org.postgresql" % "postgresql" % "9.4.1207.jre7",
  "com.typesafe.play" %% "anorm" % "2.5.0",
  "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.0",
  "com.typesafe.akka" %% "akka-contrib" % "2.3.13",
  "net.codingwell" %% "scala-guice" % "4.0.1",
  "com.iheart" %% "ficus" % "1.2.6",
  "net.ruippeixotog" %% "scala-scraper" % "2.0.0-RC2")

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Kaliber Repository" at "https://jars.kaliber.io/artifactory/libs-release-local"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator
