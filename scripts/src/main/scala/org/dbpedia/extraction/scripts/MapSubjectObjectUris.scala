package org.dbpedia.extraction.scripts

import java.io.File

import org.apache.jena.ext.com.google.common.collect.{Multimaps, TreeMultimap}
import org.dbpedia.extraction.config.ConfigUtils.parseLanguages
import org.dbpedia.extraction.util.RichFile.wrapFile
import org.dbpedia.extraction.util.{DateFinder, Language, SimpleWorkers, Workers}

import scala.Console.err
import scala.collection.convert.decorateAsScala._

/**
 * Maps subject and object URI to new subject and object URIs
 */
object MapSubjectObjectUris {

  private def split(arg: String): Array[String] = {
    arg.split(",").map(_.trim).filter(_.nonEmpty)
  }

  def main(args: Array[String]): Unit = {

    require(args != null && args.length >= 7,
      "need at least seven args: " +
        /*0*/ "base dir of the extraction" +
        /*1*/ "comma-separated names of datasets mapping old URIs to new URIs (e.g. 'transitive-redirects'), " +
        /*2*/ "mapping file suffix (e.g. '.nt.gz', '.ttl', '.ttl.bz2'), " +
        /*3*/ "comma-separated names of input datasets (e.g. 'infobox-properties,mappingbased-properties'), " +
        /*4*/ "output dataset name extension (e.g. '-redirected'), " +
        /*5*/ "comma-separated input/output file suffixes (e.g. '.nt.gz,.nq.bz2', '.ttl', '.ttl.bz2'), " +
        /*6*/ "languages or article count ranges (e.g. 'en,fr' or '10000-') or choose '@external' to map external datasets from a secondary directory (see last argument)" +
        /*7*/ "optional secondary directory (containing the input datasets to map if language option is '@external')")

    val baseDir = new File(args(0))

    val mappings = split(args(1))
    require(mappings.nonEmpty, "no mapping datasets")

    // Suffix of mapping files, for example ".nt", ".ttl.gz", ".nt.bz2" and so on.
    // This script works with .nt, .ttl, .nq or .tql files, using IRIs or URIs.
    val mappingSuffix = args(2)
    require(mappingSuffix.nonEmpty, "no mapping file suffix")

    val inputs = split(args(3))
    require(inputs.nonEmpty, "no input datasets")

    val extension = args(4)
    require(extension.nonEmpty, "no result name extension")

    // Suffixes of input/output files, for example ".nt", ".ttl.gz", ".nt.bz2" and so on.
    // This script works with .nt, .ttl, .nq or .tql files, using IRIs or URIs.
    val suffixes = split(args(5))
    require(suffixes.nonEmpty, "no input/output file suffixes")

    // Use all remaining args as keys or comma or whitespace separated lists of keys
    var isExternal = false
    val languages = if(args(6).trim == "@external") {
      isExternal = true
      Array(Language.English)
    }
    else
      parseLanguages(baseDir, split(args(6)))
    require(languages.nonEmpty, "no languages")

    val secondary = if(isExternal) new File(args(7)) else null

    // Redirects can have only one target, so we don't really need a MultiMap here.
    // But CanonicalizeUris also uses a MultiMap... TODO: Make this configurable.

    for (language <- languages) {
      val finder = new DateFinder(baseDir, language)
      val map = Multimaps.synchronizedSortedSetMultimap[String, String](TreeMultimap.create[String, String]())

        Workers.work(SimpleWorkers(1.5, 1.0) { mapping: String =>
        var count = 0
        new QuadMapper().readQuads(finder, mapping + mappingSuffix, auto = true) { quad =>
          if (quad.datatype != null) throw new IllegalArgumentException(mapping + ": expected object uri, found object literal: " + quad)
          map.put(quad.subject, quad.value)
          count += 1
        }
        err.println(mapping + ": found " + count + " mappings")
      }, mappings.toList)

      Workers.work(SimpleWorkers(1.5, 1.0) { input: (String, String) =>
        var count = 0
        val inputFile = if(isExternal) new File(secondary, input._1 + input._2) else finder.byName(input._1 + input._2, auto = true).get
        val outputFile = if(isExternal) new File(secondary, input._1 + extension + input._2) else finder.byName(input._1 + extension + input._2, auto = true).get
        new QuadMapper().mapQuads(language, inputFile, outputFile) { quad =>

          var newSubjects = map.get(quad.subject).asScala
          if(newSubjects.isEmpty)
            newSubjects.add(quad.subject)

          var newValues = if(quad.datatype != null) collection.mutable.Set[String]() else map.get(quad.value).asScala
          if(newValues.isEmpty)
            newValues.add(quad.value)

          for { s <- newSubjects; v <- newValues } //Cross product
            yield quad.copy(
              subject = s, // change subject URI
              value = v, // change object URI
              context = if (quad.context == null) quad.context else quad.context + "&objectMappedFrom=" + quad.value) // add change provenance
        }

        inputFile.delete()
        outputFile.renameTo(inputFile)

        err.println(input._1 + ": changed " + count + " quads.")
      }, inputs.flatMap(x => suffixes.map(y => (x, y))).toList)
    }
  }
}
