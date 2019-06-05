package org.dbpedia.extraction.nif

import util.control.Breaks._
import org.dbpedia.extraction.config.Config
import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.ontology.{Ontology, OntologyProperty, RdfNamespace}
import org.dbpedia.extraction.transform.{Quad, QuadBuilder}
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser.WikiPage
import org.dbpedia.iri.UriUtils

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Success}
import scala.language.reflectiveCalls

class GeneralNifExtractor (
                            context : {
                              def configFile : Config
                              def language : Language
                              def ontology : Ontology
                            },
                            wikiPage: WikiPage
                          ) {

  protected val writeLinkAnchors: Boolean = context.configFile.nifParameters.writeLinkAnchor
  protected val writeStrings: Boolean = context.configFile.nifParameters.writeAnchor
  protected val recordAbstracts: Boolean = !context.configFile.nifParameters.isTestRun  //not! will create dbpedia short and long abstracts
  protected val shortAbstractLength: Int = context.configFile.abstractParameters.shortAbstractMinLength

  private val nifContextUri = wikiPage.uri + "?dbpv=" + context.configFile.dbPediaVersion + "&nif=context"
  private val sourceUrl = wikiPage.sourceIri
  private val nifcontext = "http://dbkwik.webdatacommons.org/ontology/nifcontext"
  private val ontologyLink = "http://dbkwik.webdatacommons.org/ontology/"

  val wikiPageWikiLinkProperty = context.ontology.properties("wikiPageWikiLink")
  val wikiPageExternalLinkProperty = context.ontology.properties("wikiPageExternalLink")
  val wikiPageInterWikiLinkProperty = context.ontology.properties("wikiPageInterWikiLink")
  val wikiPageInterLanguageLinkProperty = context.ontology.properties("wikiPageInterLanguageLink")
  val labelProperty = context.ontology.properties("rdfs:label")




  protected lazy val nifContext: (String, String, String, String, String) => Quad = QuadBuilder.dynamicPredicate(context.language.isoCode,DBpediaDatasets.NifContext.encoded) _
  protected lazy val nifStructure: (String, String, String, String, String) => Quad = QuadBuilder.dynamicPredicate(context.language.isoCode,DBpediaDatasets.NifPageStructure.encoded) _
  protected lazy val nifLinks: (String, String, String, String, String) => Quad = QuadBuilder.dynamicPredicate(context.language.isoCode, DBpediaDatasets.NifTextLinks.encoded) _
  //protected lazy val rawTables: (String, String, String, String, String) => Quad = QuadBuilder.dynamicPredicate(context.language.isoCode, DBpediaDatasets.RawTables.encoded) _
  //protected lazy val equations: (String, String, String, String, String) => Quad = QuadBuilder.dynamicPredicate(context.language.isoCode, DBpediaDatasets.Equations.encoded) _

  // lazy so testing does not need ontology
  protected lazy val shortProperty: OntologyProperty = context.ontology.properties(context.configFile.abstractParameters.shortAbstractsProperty)
  protected lazy val longProperty: OntologyProperty = context.ontology.properties(context.configFile.abstractParameters.longAbstractsProperty)
  protected lazy val longQuad: (String, String, String) => Quad = QuadBuilder(context.language, DBpediaDatasets.LongAbstracts, longProperty, null) _
  protected lazy val shortQuad: (String, String, String) => Quad = QuadBuilder(context.language, DBpediaDatasets.ShortAbstracts, shortProperty, null) _

  def extractNif(tocMap: ListBuffer[NifSection], text:String): Seq[Quad] = {
    var quads = new ArrayBuffer[Quad]()

    quads ++= writeContext(text)

    for(section <- tocMap){
      quads ++= writeLongAndShortAbstract(section, text)
      quads ++= writeSection(section)
      quads ++= writeParagraphs(section, text)
    }

    quads
  }

  private def writeLongAndShortAbstract(section: NifSection, text:String):ArrayBuffer[Quad] = {
    var quads = ArrayBuffer[Quad]()
    if (recordAbstracts && section.id == "abstract" && text.length > 0) {
      val describingParagraphs = section.paragraphs//getParagraphsDescribingConcept(section, text)
      if(describingParagraphs.size > 0){
        quads += longQuad(wikiPage.uri, text.substring(describingParagraphs.head.begin.getOrElse(0), describingParagraphs.last.end.getOrElse(0)), sourceUrl) //text.substring(section.begin.getOrElse(0), section.end.getOrElse(0)), sourceUrl)
        quads += shortQuad(wikiPage.uri, getShortAbstract(describingParagraphs, text), sourceUrl) // getShortAbstract(section.paragraphs, text), sourceUrl)
      }
    }
    quads
  }

  private def getParagraphsDescribingConcept(section: NifSection, text:String):Seq[NifParagraph] = {
    val wikiTitleWords = Set() ++ wikiPage.title.decoded.toLowerCase.split("[\\s\\u202F\\u00A0]").map(s=>s.trim)
    //try to find wiki title somewhere in the beginning of a paragraph
    var best = 0.0
    var paragraphs = ArrayBuffer[NifParagraph]()
    for (p <- section.paragraphs) {
      var paragraphText = text.substring(p.begin.getOrElse(0), p.end.getOrElse(0))
      // heuristically search for the title in the first "few" words of each paragraph
      val paragraphWords = Set() ++ paragraphText.toLowerCase.split("[\\s\\u202F\\u00A0]").slice(0, wikiTitleWords.size * 3).map(s=>s.trim)

      val intersectionSize = wikiTitleWords.intersect(paragraphWords).size.toFloat/wikiTitleWords.size
      if(intersectionSize > best){
        paragraphs = ArrayBuffer[NifParagraph]()
        best = intersectionSize
      }
      paragraphs += p
    }
    paragraphs
  }

  private def getShortAbstract(paragraphs: Seq[NifParagraph], text:String): String = {
    var len = 0
    var ps: List[NifParagraph] = List()
    breakable {
      for (p <- paragraphs) {
        var paragraphLength = p.end.getOrElse(0) - p.begin.getOrElse(0)
        if (len <= shortAbstractLength || len + paragraphLength < shortAbstractLength * 3) //create short Abstract between [shortAbstractLength, shortAbstractLength*3]
        {
          ps ++= List(p)
          len += paragraphLength
        }else{
          break
        }
      }
    }
    if(ps.isEmpty)
      return ""
    var shortAbstract = text.substring(ps(0).begin.getOrElse(0), ps.last.end.getOrElse(0))
    if (len > shortAbstractLength * 4){ //only cut abstract if the first paragraph is exceedingly long
      //try to end with a fullstop character:

      var tmpShortAbstract = ""
      breakable {
        for (text <- shortAbstract.split("\\.")) {
          if (tmpShortAbstract.length <= shortAbstractLength || tmpShortAbstract.length + text.length < shortAbstractLength * 4) {
            tmpShortAbstract += text + "."
          }else{
            break
          }
        }
      }

      if(tmpShortAbstract.length > shortAbstractLength * 4){
        return shortAbstract.substring(0, shortAbstractLength * 4)
      }else{
        return tmpShortAbstract
      }
    }
    shortAbstract
  }

  private def writeContext(text:String): ArrayBuffer[Quad] = {
    var quads = ArrayBuffer[Quad]()
    quads += nifContext(nifContextUri, RdfNamespace.RDF.append("type"), RdfNamespace.NIF.append("Context"), sourceUrl, null)
    quads += nifContext(nifContextUri, RdfNamespace.NIF.append("beginIndex"), "0", sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger") )
    quads += nifContext(nifContextUri, RdfNamespace.NIF.append("endIndex"), text.length.toString, sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger") )
    quads += nifContext(nifContextUri, RdfNamespace.NIF.append("sourceUrl"), sourceUrl, sourceUrl, null)
    quads += nifContext(nifContextUri, RdfNamespace.NIF.append("isString"), text.toString(), sourceUrl, RdfNamespace.XSD.append("string"))
    quads += nifContext(nifContextUri, RdfNamespace.NIF.append("predLang"), "http://lexvo.org/id/iso639-3/" + context.language.iso639_3, sourceUrl, null)
    quads += nifContext(wikiPage.uri, nifcontext, nifContextUri, sourceUrl, null) //link between resource and nif context
    quads
  }


  private def writeSection(section: NifSection): ArrayBuffer[Quad] = {
    var quads = ArrayBuffer[Quad]()

    val sectionUri = getSectionIri(section)
    quads += nifStructure(sectionUri, RdfNamespace.RDF.append("type"), RdfNamespace.NIF.append("Section"), sourceUrl, null)
    quads += nifStructure(sectionUri, RdfNamespace.SKOS.append("notation"), section.ref, sourceUrl, RdfNamespace.RDFS.append("Literal"))
    quads += nifStructure(sectionUri, RdfNamespace.NIF.append("beginIndex"), section.begin.getOrElse(0).toString, sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger"))
    quads += nifStructure(sectionUri, RdfNamespace.NIF.append("endIndex"), section.end.getOrElse(0).toString, sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger"))
    quads += nifStructure(sectionUri, RdfNamespace.NIF.append("referenceContext"), nifContextUri, sourceUrl, null)

    //adding navigational properties
    section.prev match{
      case Some(prev) =>
        val prevSectionUri = getSectionIri(prev)
        quads += nifStructure(sectionUri, RdfNamespace.NIF.append("previousSection"), prevSectionUri, sourceUrl, null)
        quads += nifStructure(prevSectionUri, RdfNamespace.NIF.append("nextSection"), sectionUri, sourceUrl, null)
      case None =>
    }

    val topSectionUri = section.top match{
      case Some(top) =>
        getSectionIri(top)
      case None =>
        nifContextUri
    }
    quads += nifStructure(sectionUri, RdfNamespace.NIF.append("superString"), topSectionUri, sourceUrl, null)
    quads += nifStructure(topSectionUri, RdfNamespace.NIF.append("hasSection"), sectionUri, sourceUrl, null)
    if (section.prev.isEmpty)
      quads += nifStructure(topSectionUri, RdfNamespace.NIF.append("firstSection"), sectionUri, sourceUrl, null)
    if (section.next.isEmpty)
      quads += nifStructure(topSectionUri, RdfNamespace.NIF.append("lastSection"), sectionUri, sourceUrl, null)

    //adding title
    if(section.beginTitle.nonEmpty && section.endTitle.nonEmpty){
      val titleUri = getNifIri("title", section.beginTitle.get, section.endTitle.get)
      quads += nifStructure(titleUri, RdfNamespace.RDF.append("type"), RdfNamespace.NIF.append("Title"), sourceUrl, null)
      quads += nifStructure(titleUri, RdfNamespace.NIF.append("referenceContext"), nifContextUri, sourceUrl, null)
      quads += nifStructure(titleUri, RdfNamespace.NIF.append("beginIndex"), section.beginTitle.get.toString, sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger"))
      quads += nifStructure(titleUri, RdfNamespace.NIF.append("endIndex"), section.endTitle.get.toString, sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger"))
      quads += nifStructure(titleUri, RdfNamespace.NIF.append("superString"), sectionUri, sourceUrl, null)
      if(writeLinkAnchors){
        quads += nifStructure(titleUri, RdfNamespace.NIF.append("anchorOf"), section.id, sourceUrl, RdfNamespace.XSD.append("string"))
        quads += nifStructure(sectionUri, labelProperty.uri, section.id.trim, sourceUrl, RdfNamespace.XSD.append("string"))
      }

    }

    quads
  }

  private def writeParagraphs(section: NifSection, text:String): ArrayBuffer[Quad] = {
    var quads = ArrayBuffer[Quad]()
    val sectionUri = getSectionIri(section)
    var lastParagraph: Option[String] = None

    val lastIndex = section.paragraphs.indices.last
    for ((paragraphObject, i) <- section.paragraphs.zipWithIndex) {
      val paragraph = getParagraphIri(paragraphObject)

      lastParagraph.foreach(quads += nifStructure(_, RdfNamespace.NIF.append("nextParagraph"), paragraph, sourceUrl, null))

      quads += nifStructure(paragraph, RdfNamespace.RDF.append("type"), RdfNamespace.NIF.append("Paragraph"), sourceUrl, null)
      quads += nifStructure(paragraph, RdfNamespace.NIF.append("beginIndex"), paragraphObject.begin.getOrElse(0).toString, sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger"))
      quads += nifStructure(paragraph, RdfNamespace.NIF.append("endIndex"), paragraphObject.end.getOrElse(0).toString, sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger"))
      quads += nifStructure(paragraph, RdfNamespace.NIF.append("referenceContext"), nifContextUri, sourceUrl, null)
      quads += nifStructure(paragraph, RdfNamespace.NIF.append("superString"), sectionUri, sourceUrl, null)

      if (writeStrings)
        quads += nifStructure(paragraph, RdfNamespace.NIF.append("anchorOf"), text.substring(paragraphObject.begin.getOrElse(0), paragraphObject.end.getOrElse(0)), sourceUrl, RdfNamespace.XSD.append("string"))

      quads += nifStructure(sectionUri, RdfNamespace.NIF.append("hasParagraph"), paragraph, sourceUrl, null)
      if (i == 0)
        quads += nifStructure(sectionUri, RdfNamespace.NIF.append("firstParagraph"), paragraph, sourceUrl, null)
      else if (i == lastIndex)
        quads += nifStructure(sectionUri, RdfNamespace.NIF.append("lastParagraph"), paragraph, sourceUrl, null)

      quads ++= writeLinks(paragraphObject, paragraph, text)
      lastParagraph = Some(paragraph)
    }
    quads
  }


  private def writeLinks(paragraph: NifParagraph, paragraphUri:String,  text:String): ArrayBuffer[Quad] = {
    var quads = ArrayBuffer[Quad]()

    for (link <- paragraph.links) {
      if(link.begin.nonEmpty && link.end.nonEmpty) {
        var linkText = text.substring(link.begin.getOrElse(0), link.end.getOrElse(0))
        val typ = if (linkText.split(" ").length > 1) "Phrase" else "Word"
        val word = getNifIri(typ.toString.toLowerCase, link.begin.getOrElse(0), link.end.getOrElse(0))
        quads += nifLinks(word, RdfNamespace.RDF.append("type"), RdfNamespace.NIF.append(typ), sourceUrl, null)
        quads += nifLinks(word, RdfNamespace.RDF.append("type"), ontologyLink + link.linkType.toString + "Link", sourceUrl, null)
        quads += nifLinks(word, RdfNamespace.NIF.append("referenceContext"), nifContextUri, sourceUrl, null)
        quads += nifLinks(word, RdfNamespace.NIF.append("beginIndex"), link.begin.getOrElse(0).toString, sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger"))
        quads += nifLinks(word, RdfNamespace.NIF.append("endIndex"), link.end.getOrElse(0).toString, sourceUrl, RdfNamespace.XSD.append("nonNegativeInteger"))
        quads += nifLinks(word, RdfNamespace.NIF.append("superString"), paragraphUri, sourceUrl, null)
        quads += nifLinks(word, "http://www.w3.org/2005/11/its/rdf#taIdentRef", link.uri, sourceUrl, null)
        if(writeLinkAnchors)
          quads += nifLinks(word, RdfNamespace.NIF.append("anchorOf"), linkText, sourceUrl, RdfNamespace.XSD.append("string"))

        link.linkType match {
          case org.dbpedia.extraction.nif.NifLinkType.Internal => quads += new Quad(context.language, DBpediaDatasets.PageLinks, wikiPage.uri, wikiPageWikiLinkProperty, link.uri, sourceUrl, null)
          case org.dbpedia.extraction.nif.NifLinkType.InterWiki => quads += new Quad(context.language, DBpediaDatasets.InterWikiLinks, wikiPage.uri, wikiPageInterWikiLinkProperty, link.uri, sourceUrl, null)
          case org.dbpedia.extraction.nif.NifLinkType.InterLanguage => quads += new Quad(context.language, DBpediaDatasets.InterLanguageLinks, wikiPage.uri, wikiPageInterLanguageLinkProperty, link.uri, sourceUrl, null)
          case org.dbpedia.extraction.nif.NifLinkType.External => quads += new Quad(context.language, DBpediaDatasets.ExternalLinks, wikiPage.uri, wikiPageExternalLinkProperty, link.uri, sourceUrl, null)
        }
      }
    }
    quads
  }



  protected def getNifIri(nifClass: String, beginIndex: Int, endIndex: Int): String ={
    UriUtils.createURI(nifContextUri) match{
      case Success(uri) =>
        var iri = uri.getScheme + "://" + uri.getHost + (if(uri.getPort > 0) ":" + uri.getPort else "") + uri.getPath + "?"
        val m = uri.getQuery.split("&").map(_.trim).collect{ case x if !x.startsWith("nif=") => x}
        iri += m.foldRight("")(_+"&"+_) + "nif=" + nifClass + "&char=" + beginIndex + "," + endIndex
        iri.replace("?&", "?")
      case Failure(f) => throw f
    }
  }

  protected def getSectionIri(section: NifSection): String ={
    getNifIri("section", section.begin.getOrElse(0), section.end.getOrElse(0))
  }

  protected def getParagraphIri(paragraph: NifParagraph): String ={
    getNifIri("paragraph", paragraph.begin.getOrElse(0), paragraph.end.getOrElse(0))
  }
}
