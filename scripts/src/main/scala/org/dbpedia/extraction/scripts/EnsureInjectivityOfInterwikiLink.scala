package org.dbpedia.extraction.scripts

import java.io.File

import org.apache.jena.ext.com.google.common.collect.{Multimaps, TreeMultimap}
import org.dbpedia.extraction.config.ConfigUtils.parseLanguages
import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.util.RichFile.wrapFile
import org.dbpedia.extraction.util.{DateFinder, IOUtils, Language, SimpleWorkers, Workers}
import org.dbpedia.extraction.util.StringUtils.formatCurrentTimestamp

import scala.Console.err
import scala.collection.convert.decorateAsScala._
import scala.collection.JavaConversions._

/**
 * Maps subject and object URI to new subject and object URIs
 */
object EnsureInjectivityOfInterwikiLink {

  private def split(arg: String): Array[String] = {
    arg.split(",").map(_.trim).filter(_.nonEmpty)
  }

  def main(args: Array[String]): Unit = {

    require(args != null && args.length >= 4,
      "need at least four args: " +
        /*0*/ "base dir of the extraction" +
        /*1*/ "comma-separated names of datasets containing links to other wikis (e.g. 'interwiki-link-link-section'), " +
        /*2*/ "mapping file suffix (e.g. '.nt.gz', '.ttl', '.ttl.bz2'), " +
        /*3*/ "languages or article count ranges (e.g. 'en,fr' or '10000-') or choose '@external' to map external datasets from a secondary directory (see last argument)")

    val baseDir = new File(args(0))

    val mappings = split(args(1))
    require(mappings.nonEmpty, "no mapping datasets")

    // Suffix of mapping files, for example ".nt", ".ttl.gz", ".nt.bz2" and so on.
    // This script works with .nt, .ttl, .nq or .tql files, using IRIs or URIs.
    val mappingSuffix = args(2)
    require(mappingSuffix.nonEmpty, "no mapping file suffix")

    // Use all remaining args as keys or comma or whitespace separated lists of keys
    var isExternal = false
    val languages = if(args(3).trim == "@external") {
      isExternal = true
      Array(Language.English)
    }
    else
      parseLanguages(baseDir, split(args(3)))
    require(languages.nonEmpty, "no languages")

    // Redirects can have only one target, so we don't really need a MultiMap here.
    // But CanonicalizeUris also uses a MultiMap... TODO: Make this configurable.

    for (language <- languages) {
      val finder = new DateFinder(baseDir, language)
      Workers.work(SimpleWorkers(1.5, 1.0) { mapping: String =>
        val map = Multimaps.synchronizedSortedSetMultimap[String, Quad](TreeMultimap.create[String, Quad]())
        var count = 0

        val inputFile = finder.byName(mapping + mappingSuffix, true).get
        new QuadMapper().readQuads(finder.language, inputFile) { quad =>
          if (quad.datatype != null) throw new IllegalArgumentException(mapping + ": expected object uri, found object literal: " + quad)
          map.put(quad.value, quad)
          count += 1
        }
        err.println(mapping + ": found " + count + " mappings")

        val outputFile = finder.byName(mapping + "_tmp" + mappingSuffix, auto = true).get
        val writer = IOUtils.writer(outputFile, false)
        try {
          writer.write("# started "+formatCurrentTimestamp+"\n")
          for((quadObject, quads) <- map.asMap()){
            if(quads.size == 1){
              val quad = quads.head
              val sb = new StringBuilder
              sb append '<' append quad.subject append "> <" append quad.predicate append "> <" append quad.value append "> .\n"
              writer.write(sb.toString)//quadToString(quad))
            }
          }
          writer.write("# completed "+formatCurrentTimestamp+"\n")
        }
        finally writer.close()

        inputFile.delete()
        outputFile.renameTo(inputFile)
      }, mappings.toList)

    }
  }
}
