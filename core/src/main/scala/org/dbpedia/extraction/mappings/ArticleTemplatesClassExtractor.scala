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
  private val owlThing = "http://www.w3.org/2002/07/owl#Thing"
  private val rdfLangStrDt = context.ontology.datatypes("rdf:langString")

  override val datasets = Set(DBpediaDatasets.InfoboxTemplateType, DBpediaDatasets.InfoboxTemplateTypeDefinitions,
    DBpediaDatasets.TemplateType, DBpediaDatasets.TemplateTypeDefinitions)

  private val infoboxSeenClasses = HashSet[String]()
  private val seenClasses = HashSet[String]()

  override def extract(node: PageNode, subjectUri: String): Seq[Quad] = {
    //Only extract for pages from the Main namespace
    if(node.title.namespace != Namespace.Main) return Seq.empty

    //Don't extract from redirect
    if(node.isRedirect) return Seq.empty

    var quads = new ArrayBuffer[Quad]()
    var hasType = false
    var templateNodes = collectTemplatesTopLevel(node)
    for(template <- templateNodes){
      var titleLower = template.title.encoded.toLowerCase
      val indexInfoBox = titleLower.indexOf("infobox")

      if(indexInfoBox != -1){
        //var title = template.title.encoded
        //before or after: blabla_infobox or infoboxblabnla
        //var charactersBefore = indexInfoBox
        //var charactersAfter = titleLower.length - (indexInfoBox + 7)
        //if(charactersBefore > charactersAfter){
        //  title = title.substring(0, indexInfoBox)
        //}
        //else{
        //  title = title.substring(indexInfoBox + 7, title.length)
        //}
        titleLower = titleLower.replaceAll("infobox", "")
        titleLower = titleLower.replace("/", "_")
        titleLower = stripAll(titleLower, " _-")
        if(titleLower.nonEmpty) {
          var classUri = context.language.dbpediaUri + "/class/" + titleLower
          //println(titleLower)
          hasType = true
          quads += new Quad(context.language, DBpediaDatasets.InfoboxTemplateType, subjectUri, typeProperty, classUri, node.sourceIri)

          infoboxSeenClasses.synchronized {
            if (!infoboxSeenClasses.contains(classUri)) {
              var classLabel = template.title.decoded.replaceAll("(?i)infobox", "").replace("/", " ")
              classLabel = stripAll(classLabel, " _-")
              infoboxSeenClasses += classUri
              quads += new Quad(context.language, DBpediaDatasets.InfoboxTemplateTypeDefinitions, classUri, typeProperty, owlClass, node.sourceIri)
              quads += new Quad(context.language, DBpediaDatasets.InfoboxTemplateTypeDefinitions, classUri, labelProperty, classLabel, node.sourceIri, rdfLangStrDt)
            }
          }
        }
      }
    }

    if(hasType == false){
      var templateClassUri = owlThing //context.language.dbpediaUri + "/class/Thing"
      var templateClassLabel = "Thing"
      if(templateNodes.isEmpty){
        quads += new Quad(context.language, DBpediaDatasets.TemplateType, subjectUri, typeProperty, owlThing, node.sourceIri)
      }else{
        var selectedTemplateNode = templateNodes.sortBy(_.children.length).last

        var titleLower = selectedTemplateNode.title.encoded.toLowerCase
        titleLower = titleLower.replace("/", "_")
        titleLower = stripAll(titleLower, " _-")

        var templateClassUri = context.language.dbpediaUri + "/class/" + titleLower

        quads += new Quad(context.language, DBpediaDatasets.TemplateType, subjectUri, typeProperty, templateClassUri, node.sourceIri)

        seenClasses.synchronized {
          if (!seenClasses.contains(templateClassUri)) {
            var classLabel = selectedTemplateNode.title.decoded.replace("/", " ")
            classLabel = stripAll(classLabel, " _-")
            seenClasses += templateClassUri
            quads += new Quad(context.language, DBpediaDatasets.TemplateTypeDefinitions, templateClassUri, typeProperty, owlClass, node.sourceIri)
            quads += new Quad(context.language, DBpediaDatasets.TemplateTypeDefinitions, templateClassUri, labelProperty, classLabel, node.sourceIri, rdfLangStrDt)
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
