package org.dbpedia.extraction.mappings

import org.apache.spark.SparkContext
import org.dbpedia.extraction.config.provenance.DBpediaDatasets
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.util.{ExtractorUtils, Language}
import org.dbpedia.extraction.wikiparser._

import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

/**
 * Extracts labels to articles based on their title.
 */
class LabelExtractor( 
  context : {
    def ontology : Ontology
    def language : Language
  }
) 
extends WikiPageExtractor
{

  val labelProperty = context.ontology.properties("rdfs:label")
  val prefLabelProperty = "http://www.w3.org/2004/02/skos/core#prefLabel"
  
  override val datasets = Set(DBpediaDatasets.Labels)

  override def extract(page: WikiPage, subjectUri: String) : Seq[Quad] =
  {
    if(page.title.namespace != Namespace.Main && !ExtractorUtils.titleContainsCommonsMetadata(page.title)) return Seq.empty

    // TODO: use templates like {{lowercase}}, magic words like {{DISPLAYTITLE}}, 
    // remove stuff like "(1999 film)" from title...
    val label = page.title.decoded


    val quads = new ArrayBuffer[Quad]()

    if(label.isEmpty)
      return Seq.empty

    quads += new Quad(context.language, DBpediaDatasets.Labels, subjectUri, labelProperty, label, page.sourceIri, context.ontology.datatypes("rdf:langString"))

    if(!page.isRedirect && !page.isDisambiguation)
      quads += new Quad(context.language, DBpediaDatasets.Labels, subjectUri, prefLabelProperty, label, page.sourceIri, context.ontology.datatypes("rdf:langString"))

    quads
  }
}
