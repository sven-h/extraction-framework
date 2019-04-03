package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.ontology.{DBpediaNamespace, Ontology}
import org.dbpedia.extraction.transform.{Quad, QuadBuilder}
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser._

import scala.collection.mutable.{ArrayBuffer, HashSet}
import scala.language.reflectiveCalls

/**
 * This extractor extracts all templates that exist in an article.
 * This data can be used for Wikipedia administrative tasks.
 */
class InterWikiSameAsExtraction(
    context: {
     def ontology: Ontology
     def language: Language
    }
  ) extends PageNodeExtractor {

  private val sameAsProperty = context.ontology.properties("owl:sameAs")

  override val datasets = Set(DBpediaDatasets.InterWikiSameAs)

  private val quad = QuadBuilder.apply(context.language, DBpediaDatasets.InterWikiSameAs, sameAsProperty, null) _

  private val dbpediaNamespace = new DBpediaNamespace("http://dbpedia.org/resource/")

  override def extract(node: PageNode, subjectUri: String): Seq[Quad] = {
    //Only extract for pages from the Main namespace
    if(node.title.namespace != Namespace.Main) return Seq.empty
    //Don't extract from redirect and disambiguation pages
    if(node.isRedirect || node.isDisambiguation) return Seq.empty

    var quads = new ArrayBuffer[Quad]()

    //check for huge wiki:
    //https://community.fandom.com/wiki/Hub:Big_wikis

    var isWikipedia = false;
    var wikipediaTitle = "";

    var templateNodes = collectTemplatesTopLevel(node)
    //list all templates:
    //  index.php?title=Special%3AAllPages&namespace=10
    //  /api.php?action=query&format=json&list=allpages&apnamespace=10&aplimit=max
    //  /api.php?action=query&format=json&list=allpages&apnamespace=10&aplimit=max&apfrom=W

    //where are the templates used:
    // /wiki/Special:WhatLinksHere
    // /index.php?title=Special%3AWhatLinksHere&target=Template%3AWikipedia-title&namespace=0
    for(template <- templateNodes){
      val name = template.title.encoded.toLowerCase

      if(name == "wikipedia" || name == "wikipedia-title" || name == "wikilink" || name == "wp" || name =="enwp" || name =="usedwp" || name =="person-enwp"){
        //http://military.wikia.com/wiki/Template:Wikipedia
        //https://memory-alpha.fandom.com/wiki/Template:Wikipedia-title
        //https://harrypotter.fandom.com/wiki/Template:Wikilink
        //https://disney.fandom.com/wiki/Template:WP
        //http://familypedia.wikia.com/wiki/Template:EnWP
        //http://familypedia.wikia.com/wiki/Template:Usedwp
        //http://familypedia.wikia.com/wiki/Template:person-enWP
        isWikipedia = true
        wikipediaTitle = getNormalizedOptionalNodeValueOrDefault(template.property("1"), node.title.decoded)
      } else if(name == "wp-song"){
        //lyrics
        //http://lyrics.wikia.com/wiki/Template:WP-Song
        isWikipedia = true
        wikipediaTitle = getNormalizedOptionalNodeValueOrDefault(template.property("1"), "")
      }else if(name == "songheader" && wikipediaTitle.isEmpty){
        //lyrics
        wikipediaTitle = getNormalizedOptionalNodeValueOrDefault(template.property("song"), wikipediaTitle)
      }else if(name == "interlang"){
        for(lang <- template.keySet){
          var title = getNormalizedOptionalNodeValueOrDefault(template.property(lang), "")
          if(title.nonEmpty){
            Language.get(lang) match {
              case Some(x) => quads += quad(subjectUri, x.resourceUri.append(title), node.sourceIri)
              case None =>
            }
          }
        }
      }

      //familypedia:
      //http://familypedia.wikia.com/wiki/Inglewood,_California?action=edit
      //http://familypedia.wikia.com/wiki/Template:Usedwp
      //http://familypedia.wikia.com/wiki/Hanna_Emerson_(1657-1738)?action=edit
      //http://familypedia.wikia.com/wiki/Template:person-enWP
      //http://familypedia.wikia.com/wiki/Template:EnWP
      //http://familypedia.wikia.com/wiki/Template:FrWP

      //military:
      //http://military.wikia.com/wiki/7_Ps_(military_adage)?action=edit
      //http://military.wikia.com/wiki/Template:Wikipedia
      //http://military.wikia.com/wiki/Template:French_wikipedia
      //http://military.wikia.com/wiki/List_of_United_States_Navy_SEALs?action=edit
      //http://military.wikia.com/wiki/Template:Better than Wikipedia
      //??? http://military.wikia.com/wiki/Template:Wikipedia_link

      //marvel:
      //only in section links and references: https://marvel.fandom.com/wiki/United_States_of_America

      //EverQuest 2: no

      //wookiepedia:
      //https://starwars.fandom.com/wiki/Kazuda_Xiono?action=edit
      //https://starwars.fandom.com/wiki/Template:Interlang
      //https://starwars.fandom.com/wiki/Cad_Bane/Legends vs: https://starwars.fandom.com/wiki/Cad_Bane

      //yu gi oh : no
      //dr who: https://tardis.fandom.com/wiki/First_Doctor vs. https://dwlegacy.fandom.com/wiki/The_First_Doctor

      // The Ice Hockey Wiki
      // https://icehockey.fandom.com/wiki/National_Hockey_League?action=edit
      // https://icehockey.fandom.com/wiki/Template:Wikipedia

      // star wars
      //https://swfanon.fandom.com/wiki/Special:Search?query=Luke+Skywalker

      // memory beta:
      //https://memory-beta.fandom.com/wiki/Star_Trek:_The_Next_Generation
      //https://memory-beta.fandom.com/wiki/Generations?action=edit
      //https://memory-beta.fandom.com/wiki/Template:Wikipedia

      //memory alpha:
      //https://memory-alpha.fandom.com/wiki/Jean-Luc_Picard?action=edit
      //https://memory-alpha.fandom.com/wiki/Template:Wikipedia
      //https://memory-alpha.fandom.com/wiki/Template:Wikipedia-title
      //mbeta-title

      //American Football Database:
      // https://americanfootballdatabase.fandom.com/wiki/1890_Greensburg_Athletic_Association_season?veaction=edit
      // after template

      //disney:
      // https://disney.fandom.com/wiki/Mickey_Mouse?action=edit
      // https://disney.fandom.com/wiki/Template:WP

      //lego:
      // https://lego.fandom.com/wiki/Hogwarts_Castle
    }
    if(isWikipedia && wikipediaTitle.nonEmpty){
      quads += quad(subjectUri, dbpediaNamespace.append(wikipediaTitle), node.sourceIri)
    }
    quads
  }

  private def collectTemplatesTopLevel(node: Node): List[TemplateNode] = {
    node match {
      case templateNode: TemplateNode => List(templateNode)
      case _ => node.children.flatMap(collectTemplatesTopLevel)
    }
  }

  private def collectSections(node: Node): List[SectionNode] = {
    node match {
      case sectionNode: SectionNode => List(sectionNode)
      case _ => node.children.flatMap(collectSections)
    }
  }

  private def getNormalizedOptionalNodeValueOrDefault(n:Option[PropertyNode], defaultValue:String): String = {
    n match {
      case Some(x) => return getNormalizeNodeValueOrDefault(x, defaultValue)
      case None => return defaultValue
    }
  }

  private def getNormalizeNodeValueOrDefault(n:PropertyNode, defaultValue:String): String = {
    val normalizedNodeValue = normalizeString(n.propertyNodeValueToPlainText)
    if(normalizedNodeValue.isEmpty)
      return defaultValue
    else
      return normalizedNodeValue
  }

  private def normalizeString(txt:String): String = {
    return txt.replace("\n", "").replace("\r", "").trim
  }
}
