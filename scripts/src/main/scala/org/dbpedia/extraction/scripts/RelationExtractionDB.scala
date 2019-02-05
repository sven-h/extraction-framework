package org.dbpedia.extraction.scripts

import java.sql.{Connection, DriverManager, Statement}

import org.dbpedia.extraction.config.Config
import org.dbpedia.extraction.config.ConfigUtils.parseLanguages
import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.destinations.DestinationUtils
import org.dbpedia.extraction.destinations.formatters.UriPolicy
import org.dbpedia.extraction.relation.RelationExtractionJava
import org.dbpedia.extraction.util.DateFinder
import org.dbpedia.extraction.util.RichFile.wrapFile

import scala.collection.JavaConversions.asScalaSet
import scala.collection.JavaConverters._
import scala.reflect.io.File


object RelationExtractionDB {

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

  def splitQuery(queryPart: String): Map[String, String] = {
    val query_pairs = scala.collection.mutable.Map[String, String]()
    for (pair <- queryPart.split("&")) {
      val idx = pair.indexOf("=")
      var key = if (idx > 0) pair.substring(0, idx) else pair
      var value = if (idx > 0 && pair.length > idx + 1) pair.substring(idx + 1) else ""
      query_pairs += (key -> value)
    }
    query_pairs.toMap
  }

  def setupDB(stmt:Statement) :Unit = {
    stmt.executeUpdate("drop table if exists person")
    stmt.executeUpdate("create table person (id integer, name string)")
  }

  def main(args: Array[String]): Unit = {

    //https://weka.wikispaces.com/Use+WEKA+in+your+Java+code
    //https://github.com/MaLeLabTs/RegexGenerator
    //https://2018.eswc-conferences.org/wp-content/uploads/2018/02/ESWC2018_paper_136.pdf

    require(args != null && args.length >= 2,
      "need two arg: " +
        /*0*/ "base dir of the extraction" +
        /*1*/ "languages or article count ranges (e.g. 'en,fr' or '10000-')")

    val baseDirString = args(0)
    require(baseDirString.nonEmpty, "no basedir")

    val config = new Config(null, baseDirString);

    val baseDir = config.dumpDir

    val languages = parseLanguages(baseDir, split(args(0)))
    require(languages.nonEmpty, "no languages")

    val fileSuffix = getFileSuffix(config)

    //Create database
    val url = "jdbc:sqlite:sample.db" //"jdbc:mysql://localhost:8889/mysql"
    //val driver = "com.mysql.jdbc.Driver"
    var connection:Connection  = null
    var stmt:Statement  = null
    try {
      //Class.forName(driver)
      connection = DriverManager.getConnection(url)
      stmt = connection.createStatement()
    } catch {
      case e: Exception => e.printStackTrace
    }

    setupDB(stmt)


    for (language <- languages) {
      val finder = new DateFinder(baseDir, language)

      stmt.executeUpdate("DROP TABLE IF EXISTS link")
      stmt.executeUpdate("CREATE TABLE link(page STRING, start INTEGER, end INTEGER, PRIMARY KEY(page))")

      val preparedStmt = connection.prepareStatement("INSERT INTO link VALUES(?,?,?)")

      new QuadMapper().readQuads(finder, "nif-text-links" + fileSuffix, auto = true) { quad =>
        if (quad.predicate == "http://www.w3.org/2005/11/its/rdf#taIdentRef") {

          val pathQuerySep = quad.subject.indexOf("?")
          val path = quad.subject.substring(0, pathQuerySep)
          val query = quad.subject.substring(pathQuerySep + 1)
          val startEnd = splitQuery(query).getOrElse("char", "0,0").split(",")

          preparedStmt.setString(1,path)
          preparedStmt.setInt(2,startEnd(0).toInt)
          preparedStmt.setInt(3,startEnd(1).toInt)

          preparedStmt.execute()
        }
      }
    }










/*
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
*/
  }
}