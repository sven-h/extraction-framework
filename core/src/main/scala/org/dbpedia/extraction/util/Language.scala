package org.dbpedia.extraction.util

import java.util.logging.{Level, Logger}
import java.util.{Locale, MissingResourceException}

import org.dbpedia.extraction.ontology.{DBpediaNamespace, RdfNamespace}

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap


/**
 * Represents a MediaWiki instance and the language used on it. Initially, this class was
 * only used for xx.wikipedia.org instances, but now we also use it for mappings.dbpedia.org
 * and www.wikidata.org. For each language, there is only one instance of this class.
 * TODO: rename this class to WikiCode or so, distinguish between enwiki / enwiktionary etc.
 *
 * @param wikiCode "en", "de", "mappings", "wikidata", ...
 * @param isoCode "en", "de", ...
 * @param dbpediaDomain Specific DBpedia domain for this language, e.g. "en.dbpedia.org".
 * May be null, e.g. for mappings.
 * @param dbpediaUri Specific DBpedia base URI for this language, e.g. "http://en.dbpedia.org".
 * May be null, e.g. for mappings.
 * @param resourceUri Specific resource namespace for this language, e.g. "http://en.dbpedia.org/resource/"
 * or "http://www.wikidata.org/entity/". May be null, e.g. for mappings. The value is not a string.
 * Use resourceUri.append("Xy"), not string concatenation.
 * @param propertyUri Specific property namespace for this language, e.g. "http://en.dbpedia.org/property/"
 * or "http://www.wikidata.org/entity/". May be null, e.g. for mappings. The value is not a string.
 * Use propertyUri.append("xy"), not string concatenation.
 * @param baseUri URI prefix for this wiki, e.g. "http://be-x-old.wikipedia.org",
 * "http://commons.wikimedia.org", "http://mappings.dbpedia.org".
 * @param apiUri API URI for this wiki, e.g. "https://be-x-old.wikipedia.org/w/api.php",
 * "http://commons.wikimedia.org/w/api.php", "https://mappings.dbpedia.org/api.php".
 */
class Language private(
  val wikiCode: String,
  val name: String,
  val isoCode: String,
  val iso639_3: String,
  val dbpediaDomain: String,
  val dbpediaUri: String,
  val resourceUri: RdfNamespace,
  val propertyUri: RdfNamespace,
  val baseUri: String,
  val apiUri: String,
  val pages: Int
) extends java.io.Serializable
{
    val locale = new Locale(isoCode)

    /**
     * Wikipedia dump files use this prefix (with underscores), e.g. be_x_old, but
     * Wikipedia domains use the wikiCode (with dashes), e.g. http://be-x-old.wikipedia.org
     */
    val filePrefix: String = wikiCode.replace('-', '_')
    /**
     */
    override def toString: String = "wiki="+wikiCode+",locale="+locale.getLanguage

    // no need to override equals() and hashCode() - there is only one object for each value, so equality means identity.
}

object Language extends (String => Language)
    {
      implicit val wikiCodeOrdering: Ordering[Language] = Ordering.by[Language, String](_.name).reverse

      val logger: Logger = Logger.getLogger(Language.getClass.getName)

      val wikipediaLanguageUrl = "https://noc.wikimedia.org/conf/langlist"


      val wikiLanguageCodes = List("aa","ab","ace","ady","af","ak","als","am","an","ang","ar","arc","arz","as","ast","atj","av","ay","az","azb",
        "ba","bar","bat-smg","bcl","be","be-tarask","bg","bh","bi","bjn","bm","bn","bo","bpy","br","bs","bug","bxr",
        "ca","cbk-zam","cdo","ce","ceb","ch","cho","chr","chy","ckb","co","cr","crh","cs","csb","cu","cv","cy",
        "da","de","din","diq","dsb","dty","dv","dz","ee","el","eml","en","eo","es","et","eu","ext",
        "fa","ff","fi","fiu-vro","fj","fo","fr","fr-ca","frp","frr","fur","fy","ga","gag","gan","gd","gl","glk","gn","gom","got","gu","gv",
        "ha","hak","haw","he","hi","hif","ho","hr","hsb","ht","hu","hy","hz","ia","id","ie","ig","ii","ik","ilo","io","is","it","iu",
        "ja","jam","jbo","jv","ka","kaa","kab","kbd","kbp","kg","ki","kj","kk","kl","km","kn","ko","koi","kr","krc","ks","ksh","ku","kv","kw","ky",
        "la","lad","lb","lbe","lez","lg","li","lij","lmo","ln","lo","lrc","lt","ltg","lv","lzz",
        "mai","map-bms","mdf","mg","mh","mhr","mi","min","mk","ml","mn","mo","mr","mrj","ms","mt","mus","mwl","my","myv","mzn",
        "na","nah","nap","nb","nds","nds-nl","ne","new","ng","nl","nn","no","nov","nrm","nso","nv","ny","oc","olo","om","or","os",
        "pa","pag","pam","pap","pcd","pdc","pfl","pi","pie","pih","pl","pms","pnb","pnt","ps","pt","pt-br","qu",
        "rm","rmy","rn","ro","roa-rup","roa-tara","ru","rue","rw","sa","sah","sc","scn","sco","sd","se","sg","si","sk","sl","sm",
        "sn","so","sq","sr","sr-el","srn","ss","st","stq","su","sv","sw","szl","ta","tcy","te","tet","tg","th","ti","tk","tl","tlh","tn","to",
        "tpi","tr","ts","tt","tum","tw","ty","tyv","udm","ug","uk","ur","uz","val","ve","vec","vep","vi","vls","vo","wa","war","wo","wuu","xal",
        "xh","xmf","yi","yo","za","zea","zh","zh-classical","zh-cn","zh-hant","zh-hk","zh-min-nan","zh-sg","zh-tw","zh-yue","zu")



      var map: Map[String, Language] = initialseMap()

      def initialseMap(): Map[String, Language] ={
        val languages = new HashMap[String,Language]
        languages("mappings") = new Language("mappings", "Mappings", "en", "eng", "mappings.dbpedia.org", "http://mappings.dbpedia.org",
          new DBpediaNamespace("http://mappings.dbpedia.org/wiki/"), new DBpediaNamespace("http://mappings.dbpedia.org/wiki/"),
          "http://mappings.dbpedia.org", "http://mappings.dbpedia.org/api.php", 0)

        languages("wikidata") = new Language("wikidata", "Wikidata", "en", "eng", "wikidata.dbpedia.org", "http://wikidata.dbpedia.org",
          new DBpediaNamespace("http://wikidata.dbpedia.org/resource/"), new DBpediaNamespace("http://wikidata.dbpedia.org/property/"),
          "http://www.wikidata.org", "https://www.wikidata.org/w/api.php", 10000000)

        languages("commons") = new Language("commons", "Commons", "en", "eng", "commons.dbpedia.org", "http://commons.dbpedia.org",
          new DBpediaNamespace("http://commons.dbpedia.org/resource/"), new DBpediaNamespace("http://commons.dbpedia.org/property/"),
          "http://commons.wikimedia.org", "https://commons.wikimedia.org/w/api.php", 10000000)

        languages("none") = new Language("none", "No Language", "en", "eng", "", "",
          new DBpediaNamespace(""), new DBpediaNamespace(""),
          "", "", 0)

        languages("core") = new Language("core", "Core Directory", "en", "eng", "", "",
          new DBpediaNamespace(""), new DBpediaNamespace(""),
          "", "", 0)

        languages("simple") = new Language("simple", "Simple English", "en", "eng", "", "",
          new DBpediaNamespace(""), new DBpediaNamespace(""),
          "", "", 0)

        languages("sh") = new Language("sh", "Serbo-Croatian", "hbs", "hbs", "", "",
          new DBpediaNamespace(""), new DBpediaNamespace(""),
          "", "", 0)

        //"mu","prefix","sh","simple",

        for(lang <- wikiLanguageCodes)
        {
          try {
            languages(lang) = makeDbkwikLanguage(lang, "default", true)
          }
          catch{
            case mre : MissingResourceException => logger.log(Level.WARNING, "Could not create the language: " + lang)
          }
        }
        languages
      }


      def preprocessWikiBase(wikiBase: String): String ={
        //return new java.net.URI(wikiBase).getScheme()
        return wikiBase.stripPrefix("http://").stripPrefix("https://").stripSuffix("/$1").stripSuffix("/wiki")
      }

      def getLanguageFreeWikiBase(wikiBase: String): String ={
        var base = preprocessWikiBase(wikiBase)
        var splits = base.split("\\.")
        if(splits.length > 1){
          if(wikiLanguageCodes.contains(splits(0))){
            var test = splits.slice(1, splits.length).mkString(".")
            return test
          }
        }
        return base
      }

      def updateInterwikis(interwikis : scala.collection.Map[String, String]):Unit={
        for ((prefix, url) <- interwikis) {
          if(url.contains("fandom") || url.contains("wikia"))
            map(prefix) = makeDbkwikLanguage(prefix,  preprocessWikiBase(url), false)
        }
        English = map("en")
      }

      def updateAllLanguages(base: String): Unit ={
        //map.clear()//do not clear because we want to keep "mappings", "wikidata" etc.
        var wikiBase = getLanguageFreeWikiBase(base)
        for(lang <- wikiLanguageCodes)
        {
          try {
            map(lang) = makeDbkwikLanguage(lang, wikiBase, true)
          }
          catch{
            case mre : MissingResourceException => logger.log(Level.WARNING, "Could not create the language: " + lang)
          }
        }
        map("commons") = new Language("commons", "Commons", "en", "eng", "commons.dbpedia.org", "http://commons.dbpedia.org",
          new DBpediaNamespace("http://commons.dbpedia.org/resource/"), new DBpediaNamespace("http://commons.dbpedia.org/property/"),
          "http://"+wikiBase, "http://"+wikiBase + "/api.php", 10000000)

        English = map("en")
        Commons = map("commons")
    }



      def makeDbkwikLanguage(language : String, wikiBase: String, modifyBase: Boolean): Language = {
        var base = wikiBase
        if(language.equals("en") == false && modifyBase){
          base = language + "." + base
        }


        val baseDomain = "dbkwik.webdatacommons.org/" + base

        val loc = Locale.forLanguageTag(language)

        var iso3 = language
        scala.util.control.Exception.ignoring(classOf[java.util.MissingResourceException]) {
          iso3 = loc.getISO3Language
        }

        new Language(
          language.trim.toLowerCase,//val wikiCode: String,
          loc.getDisplayName.trim,                //val name: String,
          loc.getLanguage.trim,             //val isoCode: String,
          iso3.trim,            //val iso639_3: String,
          baseDomain,               //val dbpediaDomain: String,
          "http://" + baseDomain,   //val dbpediaUri: String,
          new DBpediaNamespace("http://" + baseDomain + "/resource/"), //val resourceUri: RdfNamespace,
          new DBpediaNamespace("http://" + baseDomain + "/property/"), //val propertyUri: RdfNamespace,
          "http://"+ base,               //val baseUri: String,
          "https://"+ base + "/api.php", //val apiUri: String,
          0                                 //val pages: Int
        )
      }

  /**
   * English Wikipedia
   */
  var English: Language = map("en")

  /**
   * DBpedia mappings wiki
   */
  val Mappings: Language = map("mappings")

  /**
   * Wikimedia commons
   */
  var Commons: Language = map("commons")

  /**
   * Wikimedia Wikidata
   */
  val Wikidata: Language = map("wikidata")

  /**
    * The Core Directory as a quasi language
    */
  val Core: Language = map("core")

  /**
    * Alibi Language
    */
  val None: Language = map("none")

  /**
   * Gets a language object for a Wikipedia language code.
   * Throws IllegalArgumentException if language code is unknown.
   */
  def apply(code: String) : Language = map.getOrElse(code, throw new IllegalArgumentException("unknown language code "+code))

  /**
   * Gets a language object for a Wikipedia language code, or None if given code is unknown.
   */
  def get(code: String) : Option[Language] = map.get(code)

  /**
   * Gets a language object for a Wikipedia language code, or the default if the given code is unknown.
   */
  def getOrElse(code: String, default: => Language) : Language = map.getOrElse(code, default)

}