package model

import java.io.File

import org.apache.log4j.{Level, Logger}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import com.lithium.stats._
import com.lithium.latent._

object FitHMM {

  val dir = "/Users/zaid/Documents/portfolio-optimization"

  def main(args: Array[String]): Unit = {

    val rng = new scala.util.Random(111L)

    val spark = SparkSession.builder()
      .appName("DataExploration")
      .master("local[*]")
      .getOrCreate()

    import spark.implicits._

    Logger.getRootLogger.setLevel(Level.WARN)

    val returns = spark.sparkContext.objectFile[(String, IndexedSeq[Double])](dir + "/returns-data")
      .map({ case (s, xs) => (s, xs.map(x => x * 100.0)) })
    returns.persist()
    val train = returns.map(_._2)
    train.persist()

    val estimator = UnivariateGaussian.regularizedEstimator(1.0, (0.0, 1.0))

    Logger.getRootLogger.setLevel(Level.WARN)

    val nStatesPerBlock = 5
    val nBlocks = 4
    val nStates = nStatesPerBlock * nBlocks
    val nTopicMixtures = nStatesPerBlock
    val nTopics = 5

    //val nonTerminalStates = Set(0 until nStates - 1: _*)
    //val terminalState = Set(nStates - 1)
    //val indexLabelSetMap = Map(0 -> nonTerminalStates, 1 -> terminalState)

    val transitionConstraint = TransitionConstraint.blockComplete(Array.fill(nBlocks)(nStates): _*)
    val stateTopicMixtureMap = (0 until nStates).map(s => (s, s % nStatesPerBlock)).toMap

    val numRestarts = 5
    val threshold = 1.0E-06
    val maxIter = 100

    val hiddenMarkovModel = HiddenMarkovModel(nStates, transitionConstraint, nTopicMixtures, nTopics, stateTopicMixtureMap, estimator)

    val (logLikelihood, estimatedModel) = {
      maxOf(1 to numRestarts) { _ =>
        hiddenMarkovModel.estimate(train, threshold, 10, 3, rng.nextLong())
      }
    }

    val (logLike, finalModel) = hiddenMarkovModel.estimate(train, threshold, maxIter, estimatedModel)

    println(logLike)
    println("pi")
    println(finalModel.pi)
    println()
    for (i <- 0 until nStates) {
      println(finalModel.transitionMatrix(i, ::).t)
    }

    writeDistribution(finalModel, new File(dir, "/fittedHMM" + nStates))
  }

}
