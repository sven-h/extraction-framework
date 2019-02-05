package org.dbpedia.extraction.scripts

import org.dbpedia.extraction.config.Config
import org.dbpedia.extraction.config.ConfigUtils.parseLanguages
import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.destinations.DestinationUtils
import org.dbpedia.extraction.destinations.formatters.UriPolicy
import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.util.DateFinder
import org.dbpedia.extraction.util.RichFile.wrapFile

import scala.collection.JavaConversions.asScalaSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object CreateDomainRangeAndTypes {

  val RDF_TYPE="http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  val RDFS_DOMAIN="http://www.w3.org/2000/01/rdf-schema#domain"
  val RDFS_RANGE="http://www.w3.org/2000/01/rdf-schema#range"
  val RDFS_LITERAL="http://www.w3.org/2000/01/rdf-schema#Literal"
  val OWL_THING="http://www.w3.org/2002/07/owl#Thing"

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

    for (language <- languages) {
      val finder = new DateFinder(baseDir, language)

      val domain = mutable.Map[String, mutable.Map[String, Int]]()
      val range = mutable.Map[String, mutable.Map[String, Int]]()
      val typeMap = mutable.Map[String, String]().withDefaultValue(OWL_THING)
      val untyped = mutable.Map[String, mutable.HashSet[String]]()

      new QuadMapper().readQuads(finder, "template-type" + fileSuffix, auto = true) { quad =>
        if(quad.predicate.equals(RDF_TYPE)){
          typeMap(quad.subject) = quad.value
        }
      }

      new QuadMapper().readQuads(finder, "infobox-properties" + fileSuffix, auto = true) { quad =>
        val domainMap = domain.getOrElseUpdate(quad.predicate, mutable.Map[String, Int]().withDefaultValue(0))
        domainMap(typeMap(quad.subject)) += 1

        val rangeMap = range.getOrElseUpdate(quad.predicate, mutable.Map[String, Int]().withDefaultValue(0))
        if(quad.datatype == null){
          rangeMap(typeMap(quad.value)) += 1
          if(typeMap(quad.value).equals(OWL_THING) && quad.value.startsWith("http://dbkwik.webdatacommons.org")){//untyped and in our namespace
            val ingoingPredicates = untyped.getOrElseUpdate(quad.value, new mutable.HashSet[String]())
            ingoingPredicates.add(quad.predicate)
          }
        }
        else
          rangeMap(RDFS_LITERAL) += 1
     }

      val quadsRestriction = new ListBuffer[Quad]()
      for(property <- domain.keySet.union(range.keySet)){

        domain.get(property) match{
          case Some (domain_map) =>
            quadsRestriction += new Quad(language, DBpediaDatasets.InfoboxPropertyRestrictions, property, RDFS_DOMAIN, domain_map.maxBy(_._2)._1, property, null)
          case None => {}
        }

        range.get(property) match{
          case Some (range_map) =>
            quadsRestriction += new Quad(language, DBpediaDatasets.InfoboxPropertyRestrictions, property, RDFS_RANGE, range_map.maxBy(_._2)._1, property, null)
          case None => {}
        }
      }

      val quadsTypes = new ListBuffer[Quad]()
      for((objectUri, inProperties) <- untyped){
        val allrangeTypes = mutable.Map[String, Int]().withDefaultValue(0)
        for(inprop <-inProperties){
          range.get(inprop) match{
            case Some (range_map) => {
              val maxType = range_map.maxBy(_._2)._1
              if(maxType.equals(OWL_THING) == false && maxType.equals(RDFS_LITERAL) == false)
                allrangeTypes(maxType) += 1
            }
            case None => {}
          }
        }
        if(allrangeTypes.size > 0){
          quadsTypes += new Quad(language, DBpediaDatasets.SdTypesLight, objectUri, RDF_TYPE, allrangeTypes.maxBy(_._2)._1, objectUri, null)
        }
      }



      val policies = UriPolicy.parsePolicies(config, "uri-policy")
      val formats = UriPolicy.parseFormats(config, "format", policies)
      var destination = DestinationUtils.createDatasetDestination(finder, Array(DBpediaDatasets.InfoboxPropertyRestrictions.toString), formats)
      destination.open()
      destination.write(quadsRestriction)
      destination.close()

      destination = DestinationUtils.createDatasetDestination(finder, Array(DBpediaDatasets.SdTypesLight.toString), formats)
      destination.open()
      destination.write(quadsTypes)
      destination.close()
    }

  }
}