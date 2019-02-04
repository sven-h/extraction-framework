package org.dbpedia.extraction.dump.extract

import java.net.Authenticator
import java.util.concurrent.ConcurrentLinkedQueue

import org.dbpedia.extraction.config.Config
import org.dbpedia.extraction.util.{Language, ProxyAuthenticator}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Dump extraction script.
  */
object Extraction {

  val Started = "extraction-started"

  val Complete = "extraction-complete"

  def main(args: Array[String]): Unit = {

    require(args != null && args.length >= 2 && args(0).nonEmpty && args(1).nonEmpty,
      "you don't provide 2 arguments: (1)config file name, (2)wikibase (3)dump directory (optional)")
    Authenticator.setDefault(new ProxyAuthenticator())
    var dumpDir = ""
    if(args.length >= 3 && args(2).nonEmpty){
      dumpDir = args(2)
    }
    Language.updateAllLanguages(args(1))

    //Load extraction jobs from configuration
    val config = new Config(args(0), dumpDir)
    val configLoader = new ConfigLoader(config)

    val parallelProcesses = if(config.runJobsInParallel) config.parallelProcesses else 1
    val jobsRunning = new ConcurrentLinkedQueue[Future[Unit]]()
    //Execute the extraction jobs one by one
    for (job <- configLoader.getExtractionJobs) {
      while(jobsRunning.size() >= parallelProcesses)
        Thread.sleep(1000)

      val future = Future{job.run()}
      jobsRunning.add(future)
      future.onComplete {
        case Failure(f) => throw f
        case Success(_) => jobsRunning.remove(future)
      }
    }

    while(jobsRunning.size() > 0)
      Thread.sleep(1000)
  }
}
