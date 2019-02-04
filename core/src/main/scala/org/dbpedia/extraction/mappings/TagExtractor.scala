package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.config.mappings.InfoboxExtractorConfig
import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.util.{ExtractorUtils, Language, WikiUtil}
import org.dbpedia.extraction.wikiparser._
import org.dbpedia.extraction.util.RichString.wrapString

import scala.collection.mutable.{ArrayBuffer, HashSet}
import scala.language.reflectiveCalls

/**
 * Extracts labels to articles based on their title.
 */
class TagExtractor(
  context : {
    def ontology : Ontology
    def language : Language
  }
) 
extends PageNodeExtractor
{
  private val ontology = context.ontology
  private val language = context.language
  private val SplitWordsRegex = InfoboxExtractorConfig.SplitWordsRegex
  private val TrailingNumberRegex = InfoboxExtractorConfig.TrailingNumberRegex


  private val labelProperty = ontology.properties("rdfs:label")
  private val typeProperty = ontology.properties("rdf:type")
  private val propertyClass = ontology.classes("rdf:Property")
  private val rdfLangStrDt = ontology.datatypes("rdf:langString")

  private val xmlRegex = raw"(?s)<([^>]+)>(.+?)<\/\1>".r

  private val seenProperties = HashSet[String]()
  
  override val datasets = Set(DBpediaDatasets.TagProperties, DBpediaDatasets.TagPropertiesDefinitions)

  //https://meta.wikimedia.org/wiki/Help:XML-style_tags
  private val styleTags = HashSet[String]("big", "b", "br", "blockquote", "categorytree", "center",  "charinsert", "cite", "code", "caption",
    "dynamicpagelist", "del", "div", "dl", "dt", "dd", "em", "font", "gallery", "hiero", "h1", "h2", "h3", "h4", "h5", "h6", "hr",
    "imagemap", "inputbox",  "i", "ins",  "includeonly", "li", "math", "nowiki", "noinclude", "ol", "onlyinclude", "poem", "pre", "p",
    "ref", "references", "ruby", "rt", "rb", "rp", "section", "source", "small", "sup", "span", "s", "sub",  "syntaxhighlight", "strong", "strike",
    "tt", "td", "th", "tr", "tabber", "timeline", "table", "u", "ul", "var")

  override def extract(node: PageNode, subjectUri: String) : Seq[Quad] =
  {
    //Only extract for pages from the Main namespace
    if(node.title.namespace != Namespace.Main) return Seq.empty

    //Don't extract from redirect and disambiguation pages
    if(node.isRedirect || node.isDisambiguation) return Seq.empty

    val quads = new ArrayBuffer[Quad]()
    val text = node.toPlainText//node.source
    for(m <- xmlRegex.findAllMatchIn(text)){
      val prop = m.group(1).trim
      val value = m.group(2).trim

      //if not empty and not in xml style tags
      if(!styleTags.contains(prop) && prop.nonEmpty && value.nonEmpty){
        val propertyUri = getPropertyUri(prop)
        quads += new Quad(language, DBpediaDatasets.TagProperties, subjectUri, propertyUri, value, node.sourceIri, rdfLangStrDt)

        seenProperties.synchronized
        {
          if (!seenProperties.contains(propertyUri))
          {
            val propertyLabel = getPropertyLabel(prop)
            seenProperties += propertyUri
            quads += new Quad(language, DBpediaDatasets.TagPropertiesDefinitions, propertyUri, typeProperty, propertyClass.uri, node.sourceIri)
            quads += new Quad(language, DBpediaDatasets.TagPropertiesDefinitions, propertyUri, labelProperty, propertyLabel, node.sourceIri, rdfLangStrDt)
          }
        }
      }
    }



    //page.source match {
    //  case xmlRegex(tag, text) => println(tag)
    //}
    quads
  }



  private def getPropertyUri(key : String) : String =
  {
    // convert property key to camelCase
    var result = key.toLowerCase(language.locale).trim
    result = result.toCamelCase(SplitWordsRegex, language.locale)

    // Rename Properties like LeaderName1, LeaderName2, ... to LeaderName
    result = TrailingNumberRegex.replaceFirstIn(result, "")

    result = WikiUtil.cleanSpace(result)

    language.propertyUri.append(result)
  }

  private def getPropertyLabel(key : String) : String =
  {
    // convert property key to camelCase
    var result = key

    result = result.replace("_", " ")

    // Rename Properties like LeaderName1, LeaderName2, ... to LeaderName
    result = TrailingNumberRegex.replaceFirstIn(result, "")

    result
  }

}
