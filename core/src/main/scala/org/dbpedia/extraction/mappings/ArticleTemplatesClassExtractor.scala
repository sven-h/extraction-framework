package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.config.provenance.{DBpediaDatasets, Dataset}
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser._

import scala.collection.mutable.{ArrayBuffer, HashSet}
import scala.language.reflectiveCalls

/**
 * This extractor extracts all templates that exist in an article.
 * This data can be used for Wikipedia administrative tasks.
 */
class ArticleTemplatesClassExtractor(
    context: {
     def ontology: Ontology
     def language: Language
    }
  ) extends PageNodeExtractor {

  private val typeProperty = context.ontology.properties("rdf:type")
  private val labelProperty = context.ontology.properties("rdfs:label")
  private val owlClass = "http://www.w3.org/2002/07/owl#Class"
  private val rdfLangStrDt = context.ontology.datatypes("rdf:langString")

  override val datasets = Set(DBpediaDatasets.TemplateType, DBpediaDatasets.TemplateTypeDefinitions)

  private val seenClasses = HashSet[String]()

  override def extract(node: PageNode, subjectUri: String): Seq[Quad] = {
    var quads = new ArrayBuffer[Quad]()
    for(template <- collectTemplatesTopLevel(node)){
      var titleLower = template.title.encoded.toLowerCase
      val indexInfoBox = titleLower.indexOf("infobox")

      if(indexInfoBox != -1){
        //before or after: blabla_infobox or infoboxblabnla
        var charactersBefore = indexInfoBox
        var charactersAfter = titleLower.length - (indexInfoBox + 7)
        var title = template.title.encoded
        if(charactersBefore > charactersAfter){
          title = title.substring(0, indexInfoBox)
        }
        else{
          title = title.substring(indexInfoBox + 7, title.length)
        }
        title = stripAll(title, " _-")

        var classUri = context.language.dbpediaUri + "/class/" + title
        quads += new Quad(context.language, DBpediaDatasets.TemplateType, subjectUri, typeProperty,  classUri, node.sourceIri)

        seenClasses.synchronized
        {
          if (!seenClasses.contains(classUri))
          {
            var classLabel = template.title.decoded.replaceAll("(?i)infobox", "")
            classLabel = stripAll(classLabel, " _-")
            seenClasses += classUri
            quads += new Quad(context.language, DBpediaDatasets.TemplateTypeDefinitions, classUri, typeProperty, owlClass, node.sourceIri)
            quads += new Quad(context.language, DBpediaDatasets.TemplateTypeDefinitions, classUri, labelProperty, classLabel, node.sourceIri, rdfLangStrDt)
          }
        }
      }

    }
    quads
  }

  private def collectTemplatesTopLevel(node: Node): List[TemplateNode] = {
    node match {
      case templateNode: TemplateNode => List(templateNode)
      case _ => node.children.flatMap(collectTemplatesTopLevel)
    }
  }

  private def stripAll(s: String, bad: String): String = {

    @scala.annotation.tailrec def start(n: Int): String =
      if (n == s.length) ""
      else if (bad.indexOf(s.charAt(n)) < 0) end(n, s.length)
      else start(1 + n)

    @scala.annotation.tailrec def end(a: Int, n: Int): String =
      if (n <= a) s.substring(a, n)
      else if (bad.indexOf(s.charAt(n - 1)) < 0) s.substring(a, n)
      else end(a, n - 1)

    start(0)
  }
}
