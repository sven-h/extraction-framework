package org.dbpedia.extraction.scripts

import java.util

import org.dbpedia.extraction.config.Config
import org.dbpedia.extraction.config.ConfigUtils.parseLanguages
import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.destinations.DestinationUtils
import org.dbpedia.extraction.destinations.formatters.UriPolicy
import org.dbpedia.extraction.relation.RelationExtractionJava
import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.util.DateFinder
import org.dbpedia.extraction.util.RichFile.wrapFile

import scala.collection.JavaConversions.asScalaSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.io.File
import collection.JavaConverters._

object RelationExtraction {

  private def split(arg: String): Array[String] = {
    arg.split(",").map(_.trim).filter(_.nonEmpty)
  }

  private def getFileSuffix(config: Config): String = {
    for (key <- config.stringPropertyNames) {
      if (key.startsWith("format.")) {
        return key.substring(6)
      }
    }
    return ".ttl"
  }

  def main(args: Array[String]): Unit = {

    require(args != null && args.length >= 1,
      "need one arg: " +
        /*0*/ "languages or article count ranges (e.g. 'en,fr' or '10000-')")

    val config = new Config(null);

    val baseDir = config.dumpDir

    val languages = parseLanguages(baseDir, split(args(0)))
    require(languages.nonEmpty, "no languages")

    val fileSuffix = getFileSuffix(config)

    for (language <- languages) {
      val finder = new DateFinder(baseDir, language)
      finder.byName("labels"+fileSuffix, true)
//finder.byName()

      val date = finder.finder.dates("nif-context.ttl").last

      RelationExtractionJava.baseDir = finder.finder.directory(date) + File.separator + finder.finder.wikiName+'-'+date+'-'
      RelationExtractionJava.suffix = fileSuffix

      //val quads = new ListBuffer[Quad]()
      //for(quad <- RelationExtractionJava.run(language).asScala){
      //  quads += quad
      //}


      val policies = UriPolicy.parsePolicies(config, "uri-policy")
      val formats = UriPolicy.parseFormats(config, "format", policies)
      var destination = DestinationUtils.createDatasetDestination(finder, Array(DBpediaDatasets.RelationExtraction.toString), formats)
      destination.open()
      destination.write(RelationExtractionJava.run(language).asScala)
      destination.close()

    }

  }
}