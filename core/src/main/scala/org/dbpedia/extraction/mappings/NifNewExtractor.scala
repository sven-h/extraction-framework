package org.dbpedia.extraction.mappings

import java.io.PrintWriter
import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import de.fau.cs.osr.ptk.common.AstPrinter
import org.apache.commons.lang3.StringEscapeUtils
import org.dbpedia.extraction.annotations.ExtractorAnnotation
import org.dbpedia.extraction.config.Config
import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.nif._
import org.dbpedia.extraction.ontology.{Ontology, OntologyProperty}
import org.dbpedia.extraction.transform.{Quad, QuadBuilder}
import org.dbpedia.extraction.util.{Language, WikiSettings}
import org.dbpedia.extraction.wikiparser._
import org.sweble.wikitext.engine._
import org.sweble.wikitext.engine.config.{I18nAliasImpl, InterwikiImpl, WikiConfig}
import org.sweble.wikitext.engine.output.{HtmlRenderer, HtmlRendererCallback, MediaInfo}
import org.sweble.wikitext.engine.utils.DefaultConfigEnWp
import org.sweble.wikitext.parser.nodes.{WtNode, WtSection, WtUrl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls
import scala.util.matching.Regex

/**
  * Extracts page html.
  *
  *
  * This class produces all nif related datasets for the abstract as well as the short-, long-abstracts datasets.
  * Where the long abstracts is the nif:isString attribute of the nif instance representing the abstract section of a wikipage.
  */

@ExtractorAnnotation("nif sweble extractor")
class NifNewExtractor(
                    context : {
                      def ontology : Ontology
                      def language : Language
                      def configFile : Config
                      def templates : Template
                      def wikiSettings : WikiSettings
                    }
                  )
  extends WikiPageExtractor
{
  override val datasets = Set(
    DBpediaDatasets.NifContext,DBpediaDatasets.NifPageStructure,DBpediaDatasets.NifTextLinks,
    DBpediaDatasets.RawTables, DBpediaDatasets.Equations,
    DBpediaDatasets.LongAbstracts, DBpediaDatasets.ShortAbstracts,
    DBpediaDatasets.InterWikiLinks, DBpediaDatasets.ExternalLinks, DBpediaDatasets.PageLinks, DBpediaDatasets.InterLanguageLinks
  )

  var config: WikiConfig = getSwebleConfig()
  var engine = new WtEngineImpl(config)

  def getSwebleConfig(): WikiConfig = {
    import collection.JavaConverters._

    var config = DefaultConfigEnWp.generate()
    config.getParserConfig.setAutoCorrect(true)
    //config.getParserConfig.setFosterParenting(true)
    //config.getParserConfig.setFosterParentingForTransclusions(true)

    for ((prefix, url) <- context.wikiSettings.interwikis) {
      if(config.getInterwiki(prefix) == null)
        config.addInterwiki(new InterwikiImpl(prefix, url, context.wikiSettings.interwikiLocal.getOrElse(prefix, false),false))
    }
    for ((name, alias) <- context.wikiSettings.magicwords) {
      if (config.getI18nAliasById(name) == null && alias.forall(p => config.getI18nAlias(p) == null))
        config.addI18nAlias(new I18nAliasImpl(name, false, alias.asJavaCollection))
    }
    config
  }


  override def extract(pageNode : WikiPage, subjectUri : String): Seq[Quad] =
  {
    //Only extract abstracts for pages from the Main namespace
    if(pageNode.title.namespace != Namespace.Main) return Seq.empty

    //Don't extract abstracts from redirect and disambiguation pages
    if(pageNode.isRedirect || pageNode.isDisambiguation) return Seq.empty

    //Retrieve html page text
    val pageTitle = PageTitle.make(config, pageNode.title.decodedWithNamespace)
    val pageId = new PageId(pageTitle, pageNode.id)

    var source = StringEscapeUtils.unescapeXml(pageNode.source)
    source = source.replaceAll("__\\S*?__", "")

    var quads = new ArrayBuffer[Quad]()

    //scala.util.control.Exception.ignoring(classOf[Exception]) {
      var page = engine.postprocess(pageId, source,new TemplateExpansionCallback(context.templates)).getPage
      //val page = engine.postprocess(pageId, source, null).getPage
      //new PrintWriter(URLEncoder.encode(pageNode.title.encoded + "_ast_expansion", StandardCharsets.UTF_8.toString)) { write(AstPrinter.print[WtNode](page)); close }
      var astVisitor = new NifExtractionAstVisitor(context.language)
      astVisitor.go(page)
      if(astVisitor.getFullText().trim.length == 0){
        //don't expand templates
        page = engine.postprocess(pageId, source,null).getPage
        astVisitor = new NifExtractionAstVisitor(context.language)
        astVisitor.go(page)
      }
      //extractionInfoPrinter(astVisitor, URLEncoder.encode(pageNode.title.encoded + "_nif_extractor", StandardCharsets.UTF_8.toString))
      quads ++= new GeneralNifExtractor(context, pageNode).extractNif(astVisitor.getTocMap(), astVisitor.getFullText())

    //}
    quads
  }

  private def extractionInfoPrinter(extractor : NifExtractionAstVisitor, fileName: String):Unit = {
    val writer = new PrintWriter(fileName)
    var text = extractor.getFullText()
    for(section <- extractor.getTocMap()){
      writer.println("Section %s (%d, %d) (prev:%s, next:%s, top:%s, sub:%s) header:%s".format(
        section.ref, section.begin.getOrElse(0), section.end.getOrElse(0),
        getNavigation(section.prev),getNavigation(section.next), getNavigation(section.top), getNavigation(section.sub),
        section.id))

      for (paragraph <- section.paragraphs) {
        writer.println("Paragraph (%d,%d):%s".format(paragraph.begin.getOrElse(0),paragraph.end.getOrElse(0), text.substring(paragraph.begin.getOrElse(0), paragraph.end.getOrElse(0)).replace("\n", "\\n")))

        for(l <- paragraph.links){
          writer.println("\tLink%s(%d,%d,\"%s\")->\"%s\"".format(l.linkType, l.begin.getOrElse(0), l.end.getOrElse(0), text.substring(l.begin.getOrElse(0), l.end.getOrElse(0)), l.uri))
        }
        //writer.println("\t" + paragraph.links.map(
        //  l=>"Link%s(%d,%d,\"%s\")->\"%s\"".format(l.linkType, l.begin.getOrElse(0), l.end.getOrElse(0), text.substring(l.begin.getOrElse(0), l.end.getOrElse(0)), l.uri)
        //).mkString(" "))
      }
      writer.println()
    }
    writer.close()
  }
  private def getNavigation(sec : Option[NifSection]):String={
    if(sec.nonEmpty)
      return sec.get.ref
    return "-"
  }
}


final private class TemplateExpansionCallback(templates : Template) extends ExpansionCallback {
  override def retrieveWikitext(expansionFrame: ExpansionFrame, pageTitle: PageTitle): FullPage =templates.getFullPage(pageTitle.getTitle)
  override def fileUrl(pageTitle: PageTitle, width: Int, height: Int): String = ""
}