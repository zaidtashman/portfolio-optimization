name := "volatility"

version := "0.1"

scalaVersion := "2.11.12"

val SPARK_VERSION = "2.4.0"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % SPARK_VERSION,
  "org.apache.spark" %% "spark-sql" % SPARK_VERSION,
  "org.apache.spark" %% "spark-mllib" % SPARK_VERSION,
  "joda-time" % "joda-time" % "2.7",
  "org.joda" % "joda-convert" % "1.7",
  "org.apache.commons" % "commons-math3" % "3.6",
  "org.scalatest" %% "scalatest" % "2.2.4" % Test,
  "org.slf4j" % "slf4j-nop" % "1.7.6" % Test,
  "org.json4s" %% "json4s-jackson" % "3.2.11" % "provided",
  "org.scalanlp" %% "breeze" % "0.13.1",
  "org.scalanlp" %% "breeze-natives" % "0.13.1",
  "com.lithium" %% "lithium" % "0.2",
  "org.scalaj" % "scalaj-http_2.11" % "2.3.0",
  "net.liftweb" %% "lift-json" % "3.1.1"
)