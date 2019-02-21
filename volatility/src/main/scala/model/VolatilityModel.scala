package model

import org.apache.spark.sql.SparkSession
import org.apache.log4j.{Level, Logger}
import scalaj.http.Http

import scala.collection.immutable.Map.Map3
import net.liftweb.json.DefaultFormats
import net.liftweb.json._
import org.apache.spark.rdd.RDD

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer


object VolatilityModel {

  val dir = "/Users/zaid/Documents/portfolio-optimization"

  def main(args: Array[String]): Unit = {

    val rng = new scala.util.Random(111L)

    val spark = SparkSession.builder()
      .appName("VolatilityModel")
      .master("local[*]")
      .getOrCreate()

    import spark.implicits._

    Logger.getRootLogger.setLevel(Level.WARN)

    val raw = spark.read
      .option("header", "true")
      .option("inferSchema", "true")
      .csv(dir+ "/anr-no-volatility.csv")
      .as[(Int, String, Double, Double, String, Double, Double, Double)]

    val histData: RDD[(String, immutable.IndexedSeq[Double])] = raw.rdd.map({ case (_, _, _, _, symbol, _, _, _) =>
      val period = 5
      val res = Http("https://api.tdameritrade.com/v1/marketdata/" + symbol + "/pricehistory?apikey=OFWNX9PJR###&periodType=year&period=" + period + "&frequencyType=daily&frequency=1").asString
      println(symbol + " " + res.body)
      Thread.sleep(1000)
      val obj = parse(res.body)
      val candles = (obj \\ "candles").children
      val close = for (i <- 0 until candles(0).children.length) yield {
        candles(0)(i).values.asInstanceOf[Map[String, Double]]("close")
      }
      (symbol, close.sliding(2).map({ case c => (c(1) - c(0)) / c(0) }).toIndexedSeq)
    })

    histData.saveAsObjectFile(dir+"/returns-data")

  }

}
