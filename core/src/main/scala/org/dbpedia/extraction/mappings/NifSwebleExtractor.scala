package org.dbpedia.extraction.mappings

import de.fau.cs.osr.ptk.common.AstPrinter
import java.io.PrintWriter

import org.dbpedia.extraction.annotations.ExtractorAnnotation
import org.dbpedia.extraction.config.Config
import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.nif.{TextConvert, WikipediaNifExtractor}
import org.dbpedia.extraction.ontology.{Ontology, OntologyProperty}
import org.dbpedia.extraction.transform.{Quad, QuadBuilder}
import org.dbpedia.extraction.util.{Language, WikiSettings}
import org.dbpedia.extraction.wikiparser._
import org.sweble.wikitext.engine._
import org.sweble.wikitext.engine.config.{I18nAliasImpl, InterwikiImpl, WikiConfig, WikiConfigImpl}
import org.sweble.wikitext.engine.utils.DefaultConfigEnWp
import org.sweble.wikitext.engine.output.HtmlRenderer
import org.sweble.wikitext.engine.output.HtmlRendererCallback
import org.sweble.wikitext.engine.output.MediaInfo
import org.sweble.wikitext.engine.utils.UrlEncoding
import org.sweble.wikitext.parser.nodes.{WtNode, WtUrl}
import org.apache.commons.lang3.StringEscapeUtils
import org.sweble.wikitext.engine.nodes.{EngPage, EngProcessedPage}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

/**
  * Extracts page html.
  *
  *
  * This class produces all nif related datasets for the abstract as well as the short-, long-abstracts datasets.
  * Where the long abstracts is the nif:isString attribute of the nif instance representing the abstract section of a wikipage.
  */

@ExtractorAnnotation("nif sweble extractor")
class NifSwebleExtractor(
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
  override val datasets = Set(DBpediaDatasets.NifContext,DBpediaDatasets.NifPageStructure,DBpediaDatasets.NifTextLinks,DBpediaDatasets.LongAbstracts, DBpediaDatasets.ShortAbstracts, DBpediaDatasets.RawTables, DBpediaDatasets.Equations, DBpediaDatasets.InterWikiLinks)

  var config: WikiConfig = getSwebleConfig()
  var engine = new WtEngineImpl(config)
  //var textConverter =

  protected val shortAbstractLength: Int = context.configFile.abstractParameters.shortAbstractMinLength

  protected lazy val shortProperty: OntologyProperty = context.ontology.properties(context.configFile.abstractParameters.shortAbstractsProperty)
  protected lazy val longProperty: OntologyProperty = context.ontology.properties(context.configFile.abstractParameters.longAbstractsProperty)

  val wikiPageWikiLinkProperty = context.ontology.properties("wikiPageInterWikiLink")
  private val interWikiQuad = QuadBuilder.apply(context.language, DBpediaDatasets.InterWikiLinks, wikiPageWikiLinkProperty, null) _

  def getSwebleConfig(): WikiConfig = {
    //https://github.com/sweble/sweble-wikitext/blob/develop/sweble-wikitext-components-parent/swc-engine/src/main/java/org/sweble/wikitext/engine/utils/LanguageConfigGenerator.java

    import collection.JavaConverters._

    var config = DefaultConfigEnWp.generate()
    config.getParserConfig.setAutoCorrect(true)

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

  def getShortAbstract(text: String): String = {
    val builder = StringBuilder.newBuilder
    for (p <- text.split("\\.")) {
      if (builder.length <= shortAbstractLength || builder.length + p.length < shortAbstractLength * 3)
      {
        builder.append(p)
        builder.append(".")
      }
    }
    builder.toString()
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

    val source = StringEscapeUtils.unescapeXml(pageNode.source)

    var quads = new ArrayBuffer[Quad]()

    scala.util.control.Exception.ignoring(classOf[Exception]) {
      val page = engine.postprocess(pageId, source,new MyExpansionCallback(context.templates)).getPage

      var linkExtract = new SwebleLinkExtractor()
      linkExtract.go(page)
      for (link <- linkExtract.internalLinks){
          try
          {
            val destinationTitle = WikiTitle.parse(link, context.language)
            if(destinationTitle.language != context.language && destinationTitle.namespace.code == 0) {
              quads += interWikiQuad(subjectUri, destinationTitle.resourceIri, pageNode.sourceIri)
              //println(destinationTitle)
            }
          }
          catch { case _: Throwable  =>  }
      }

      //new PrintWriter(pageNode.title.decoded + "_ast_expansion") { write(AstPrinter.print[WtNode](page)); close }
      var html = HtmlRenderer.print(new MyRendererCallback, config, pageTitle, page)
      html = StringEscapeUtils.unescapeXml(html)
      //parser currently do not remove magic words:
      html = html.replaceAll("__.*?__", "")
      //new PrintWriter(pageNode.title.decoded + "_html") { write(html); close }
      quads ++= new WikipediaNifExtractor(context, pageNode).extractNif(html)(err => pageNode.addExtractionRecord(err))
    }

    //Backup for short and long abstracts
    if(!quads.exists(q=>q.predicate==shortProperty.uri||q.predicate==longProperty.uri)) scala.util.control.Exception.ignoring(classOf[Exception]) {
      scala.util.control.Exception.ignoring(classOf[Exception]) {
        val page = engine.postprocess(pageId, source,null).getPage
        //new PrintWriter(pageNode.title.decoded + "_ast") { write(AstPrinter.print[WtNode](page)); close }
        var text = new TextConvert(config).go(page).asInstanceOf[String]
        text = text.replaceAll("__.*?__", "")
        //new PrintWriter(pageNode.title.decoded + "_text") { write(text); close }
        quads += new Quad(context.language, DBpediaDatasets.ShortAbstracts, subjectUri, shortProperty,  getShortAbstract(text), pageNode.uri)
        quads += new Quad(context.language, DBpediaDatasets.LongAbstracts, subjectUri, longProperty, text, pageNode.uri)
      }
    }

    quads
  }
}

import de.fau.cs.osr.ptk.common.AstVisitor
import org.sweble.wikitext.parser.nodes.WtNode
import org.sweble.wikitext.parser.nodes.WtExternalLink
import org.sweble.wikitext.parser.nodes.WtInternalLink

class SwebleLinkExtractor() extends AstVisitor[WtNode] {

  val internalLinks = mutable.MutableList[String]()

  def visit(n: WtNode): Unit = { // Fallback for all nodes that are not explicitly handled below
    iterate(n)
  }

  /*
  def visit(link: WtExternalLink): Unit = {
    println(link)
  } */

  def visit(link: WtInternalLink): Unit = {
    //var prefix = link.getPrefix
    //var resolved = link.getTarget.isResolved
    var target = link.getTarget.getAsString
    internalLinks += target
    //println(target)
  }

}

final private class MyExpansionCallback(templates : Template) extends ExpansionCallback {
  override def retrieveWikitext(expansionFrame: ExpansionFrame, pageTitle: PageTitle): FullPage =templates.getFullPage(pageTitle.getTitle)
  override def fileUrl(pageTitle: PageTitle, width: Int, height: Int): String = ""
}

final private class MyRendererCallback extends HtmlRendererCallback {
  override def resourceExists(target: PageTitle): Boolean = true
  override def getMediaInfo(title: String, width: Int, height: Int): MediaInfo = null
  override def makeUrl(target: PageTitle): String = {
    val page = UrlEncoding.WIKI.encode(target.getNormalizedFullTitle)
    val f = target.getFragment
    var url = page
    if (f != null && !f.isEmpty) url = page + "#" + UrlEncoding.WIKI.encode(f)
    "/mediawiki/" + url
  }
  override def makeUrl(target: WtUrl): String = {
    if (target.getProtocol eq "") return target.getPath
    target.getProtocol + ":" + target.getPath
  }
  override def makeUrlMissingTarget(path: String): String = {
    "/mediawiki/?title=" + path + "&amp;action=edit&amp;redlink=1"
  }
}