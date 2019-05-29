package org.dbpedia.extraction.nif

import scala.collection.mutable.ListBuffer

class NifSection(
                  var id: String,
                  var ref: String,
                  var prev: Option[NifSection],
                  var next: Option[NifSection],
                  var top: Option[NifSection],
                  var sub: Option[NifSection],
                  var begin: Option[Int],
                  var end: Option[Int],
                  var paragraphs: ListBuffer[NifParagraph]
                ) {
  def addParagraph(nifparagraph: NifParagraph): Unit = paragraphs += nifparagraph

}
