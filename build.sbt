import scala.language.postfixOps
import scala.sys.process.*

val Http4sVersion = "0.23.28"
val CirceVersion = "0.14.9"
val MunitVersion = "1.0.0"
val LogbackVersion = "1.5.6"
val MunitCatsEffectVersion = "2.0.0"

lazy val buildDockerImage = taskKey[Unit]("Build Docker image for project")

// sbt task to test build locally
buildDockerImage := {
  println("Running assembly task")
  assembly.value
  println("Starting Docker image build")
  "docker build . -t financial-market-data-puller-test" !
}

lazy val root = (project in file("."))
  .settings(
    organization := "com.crisszkutnik",
    name := "financial-market-data-puller",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "3.3.3",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-ember-server" % Http4sVersion,
      "org.http4s"      %% "http4s-ember-client" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "org.scalameta"   %% "munit"               % MunitVersion           % Test,
      "org.typelevel"   %% "munit-cats-effect"   % MunitCatsEffectVersion % Test,
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion         % Runtime,
    ),
    libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M13",
    libraryDependencies += "org.jsoup" % "jsoup" % "1.18.1",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.24.1",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "3.3.1",
    libraryDependencies ++= Seq(
      "org.apache.poi" % "poi" % "5.3.0",
      "org.apache.poi" % "poi-ooxml" % "5.3.0",
      "org.apache.poi" % "poi-ooxml-lite" % "5.3.0"
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-generic" % "0.14.8",
      "io.circe" %% "circe-literal" % "0.14.8"
    ),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", x, xs @ _*) if x.toLowerCase == "services" => MergeStrategy.filterDistinctLines
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    },
    assembly / assemblyJarName := "application.jar",
  )