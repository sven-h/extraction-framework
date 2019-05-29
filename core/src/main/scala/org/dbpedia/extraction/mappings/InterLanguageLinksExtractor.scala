package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.transform.{QuadBuilder, Quad}
import org.dbpedia.extraction.wikiparser._
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.{Language, ExtractorUtils}
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls


/**
 * Extracts interwiki links
 */
class InterLanguageLinksExtractor(context: { def ontology : Ontology; def language : Language }) extends PageNodeExtractor
{
  private val wikiPageInterLanguageLinkProperty = context.ontology.properties("wikiPageInterLanguageLink")

  override val datasets = Set(DBpediaDatasets.InterLanguageLinks,DBpediaDatasets.InterWikiLinks)
  
  private val namespaces = if (context.language == Language.Commons) ExtractorUtils.commonsNamespacesContainingMetadata
    else Set(Namespace.Main)//Set(Namespace.Main, Namespace.Template, Namespace.Category)
  
  private val quadInterLang = QuadBuilder.apply(context.language, DBpediaDatasets.InterLanguageLinks, wikiPageInterLanguageLinkProperty, null) _
  val wikiPageWikiLinkProperty = context.ontology.properties("wikiPageInterWikiLink")
  private val quadInterWiki = QuadBuilder.apply(context.language, DBpediaDatasets.InterWikiLinks, wikiPageWikiLinkProperty, null) _

  override def extract(page : PageNode, subjectUri : String) : Seq[Quad] =
  {
    if (! namespaces.contains(page.title.namespace)) return Seq.empty
    
    var quads = new ArrayBuffer[Quad]()

    for (node <- page.children) { // was page.children.reverse - why?
      node match {
        case link: InterWikiLinkNode => {
          val dst = link.destination
          if (dst.isInterLanguageLink) {
            quads += quadInterLang(subjectUri, dst.language.resourceUri.append(dst.decodedWithNamespace), link.sourceIri)
          }else if(dst.language.dbpediaUri != context.language.dbpediaUri){
            quads += quadInterWiki(subjectUri, dst.language.resourceUri.append(dst.decodedWithNamespace), link.sourceIri)
          }
        }
        case _ => // ignore
      }
    }
    
    quads
  }

}
