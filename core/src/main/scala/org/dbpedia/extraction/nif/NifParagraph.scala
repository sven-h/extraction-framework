package org.dbpedia.extraction.nif

import scala.collection.mutable.ListBuffer

class NifParagraph(
                    var begin: Option[Int],
                    var end: Option[Int],
                    var links: ListBuffer[NifLink]
                  ) {

  def addLink(link: NifLink): Unit = links += link
}
