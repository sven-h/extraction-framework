package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.annotations.ExtractorAnnotation
import org.dbpedia.extraction.config.Config
import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.nif.WikipediaNifExtractor
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.util.{Language, WikiSettings}
import org.dbpedia.extraction.wikiparser._
import org.sweble.wikitext.engine._
import org.sweble.wikitext.engine.config.{I18nAliasImpl, InterwikiImpl, WikiConfig, WikiConfigImpl}
import org.sweble.wikitext.engine.utils.DefaultConfigEnWp
import org.sweble.wikitext.engine.output.HtmlRenderer
import org.sweble.wikitext.engine.output.HtmlRendererCallback
import org.sweble.wikitext.engine.output.MediaInfo
import org.sweble.wikitext.engine.utils.UrlEncoding
import org.sweble.wikitext.parser.nodes.WtUrl
import org.apache.commons.lang3.StringEscapeUtils

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
  var config: WikiConfig = getSwebleConfig()
  var engine = new WtEngineImpl(config)

  def getSwebleConfig(): WikiConfig = {
    //https://github.com/sweble/sweble-wikitext/blob/develop/sweble-wikitext-components-parent/swc-engine/src/main/java/org/sweble/wikitext/engine/utils/LanguageConfigGenerator.java

    import collection.JavaConverters._

    var config = DefaultConfigEnWp.generate()

    for ((prefix, url) <- context.wikiSettings.interwikis) {
      if(config.getInterwiki(prefix) == null)
        config.addInterwiki(new InterwikiImpl(prefix, url, url.contains(".wikia.com"),false))
    }
    for ((name, alias) <- context.wikiSettings.magicwords) {
      if (config.getI18nAliasById(name) == null && alias.forall(p => config.getI18nAlias(p) == null))
        config.addI18nAlias(new I18nAliasImpl(name, false, alias.asJavaCollection))
    }
    //TODO: namespace alisas and namespace
    config
  }


  override val datasets = Set(DBpediaDatasets.NifContext,DBpediaDatasets.NifPageStructure,DBpediaDatasets.NifTextLinks,DBpediaDatasets.LongAbstracts, DBpediaDatasets.ShortAbstracts, DBpediaDatasets.RawTables, DBpediaDatasets.Equations)

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

    try {
      val cp = engine.postprocess(pageId, source, new MyExpansionCallback(context.templates))
      var html = HtmlRenderer.print(new MyRendererCallback, config, pageTitle, cp.getPage)
      html = StringEscapeUtils.unescapeXml(html)
      //parser currently do not remove magic words:
      html = html.replace("__NOTOC__", "")
      return new WikipediaNifExtractor(context, pageNode).extractNif(html)(err => pageNode.addExtractionRecord(err))
    }
    catch {
      case ex: Exception => return Seq.empty
    }
    return Seq.empty
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