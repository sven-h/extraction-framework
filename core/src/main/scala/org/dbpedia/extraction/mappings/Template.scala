package org.dbpedia.extraction.mappings

import org.apache.commons.lang3.StringEscapeUtils
import org.dbpedia.extraction.sources.Source
import org.dbpedia.extraction.wikiparser.Namespace
import org.sweble.wikitext.engine.{FullPage, PageId, PageTitle}
import org.sweble.wikitext.engine.utils.DefaultConfigEnWp

class Template (val map : Map[String, FullPage])
{
  def getFullPage(title : String) : FullPage = {
    map.getOrElse(title, null)
  }
}
object Template{

  def load(articlesSource : Source) : Template =
  {
    val config = DefaultConfigEnWp.generate()
    val template_map = scala.collection.mutable.Map[String, FullPage]()
    for (page <- articlesSource){
      if(page.title.namespace == Namespace.Template){
        val pageId = new PageId(PageTitle.make(config, page.title.decodedWithNamespace), page.id)
        val template = new FullPage(pageId, StringEscapeUtils.unescapeXml(page.source))
        template_map(page.title.encoded) = template
      }
    }
    new Template(template_map.toMap)
  }


}