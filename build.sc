import mill._
import mill.scalalib._
import mill.scalalib.scalafmt.ScalafmtModule

object app extends ScalaModule with ScalafmtModule {

  def scalaVersion = "2.13.4"

  val akkaVersion = "2.6.8"
  val akkaHttpVersion = "10.2.3"

  override def ivyDeps = Agg(
    ivy"com.typesafe.akka::akka-actor-typed:${akkaVersion}",
    ivy"com.typesafe.akka::akka-stream-typed:${akkaVersion}",
    ivy"com.typesafe.akka::akka-http:${akkaHttpVersion}",
    ivy"com.lihaoyi::scalatags:0.8.2",
    ivy"com.google.zxing:core:3.4.1",
    ivy"ch.qos.logback:logback-classic:1.2.3"
  )
}
