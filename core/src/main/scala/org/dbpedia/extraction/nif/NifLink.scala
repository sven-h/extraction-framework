package org.dbpedia.extraction.nif

class NifLink(
               var begin: Option[Int],
               var end: Option[Int],
               var uri: String,
               var linkType: NifLinkType.Value,
               var wikiTarget : String
             ) {
}


object NifLinkType extends Enumeration {
  type NifLinkType = Value
  val Internal, InterWiki, InterLanguage, External = Value
}