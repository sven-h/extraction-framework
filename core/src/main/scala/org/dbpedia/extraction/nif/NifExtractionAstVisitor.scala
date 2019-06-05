package org.dbpedia.extraction.nif

import de.fau.cs.osr.ptk.common.AstVisitor
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser.WikiTitle
import org.sweble.wikitext.engine.nodes.EngPage
import org.sweble.wikitext.engine.nodes.EngNowiki
import org.sweble.wikitext.parser.nodes.{WtUrl, _}

import scala.collection.mutable.ListBuffer


class NifExtractionAstVisitor(language : Language)
  extends AstVisitor[WtNode] {

  private var tocMap = new ListBuffer[NifSection]()
  private var currentSection = new ListBuffer[Int]()

  private var context = StringBuilder.newBuilder //contains the whole text of the wikipage
  private var nifSection: NifSection = new NifSection(id = "abstract",ref = "", prev = None, next = None, top = None, sub = None, begin = Some(0), end = None, beginTitle = None, endTitle = None, paragraphs = ListBuffer())
  private var nifParagraph: NifParagraph = new NifParagraph(begin = Some(0), end = None,links = ListBuffer())
  private var extLinkNum :Int = 1

  //private var categoryLinks: ListBuffer[String] = new ListBuffer()
  //def getCategoryLinks():ListBuffer[String] = { categoryLinks }
  def getTocMap():ListBuffer[NifSection] = { tocMap }
  def getFullText():String = { context.toString() }


  override protected def before(node: WtNode): WtNode = {
    // This method is called by go() before visitation starts

    currentSection = new ListBuffer[Int]()
    currentSection.append(0) //initialize on abstract section

    var abstractSection = new NifSection(
      id = "abstract",
      ref = currentSection.map(n => "." + n.toString).foldRight("")(_+_).substring(1),
      prev = None,
      next = None,
      top = None,
      sub = None,
      begin = Some(0),
      end = None,
      beginTitle = None,
      endTitle = None,
      paragraphs = ListBuffer()
    )
    nifSection = abstractSection
    tocMap = new ListBuffer[NifSection]()
    tocMap.append(abstractSection)
    //categoryLinks = new ListBuffer()
    extLinkNum = 1

    val firstParagraph = new NifParagraph(
      begin = Some(0),
      end = None,
      links = ListBuffer()
    )
    nifSection.addParagraph(firstParagraph)
    nifParagraph = firstParagraph

    super.before(node)
  }

  override protected def after(node: WtNode, result: Any): AnyRef = {
    nifParagraph.end = Some(context.length)
    nifSection.end = Some(context.length)

    super.after(node, result)
  }



  def visit(n: WtNode): Unit = {
    write(n.getNodeName)
  }

  def visit(n: EngNowiki): Unit = {
    write(n.getContent)
  }

  def visit(n: WtNodeList): Unit = {
    iterate(n)
  }

  def visit(e: WtUnorderedList): Unit = {
    iterate(e)
  }

  def visit(e: WtOrderedList): Unit = {
    iterate(e)
  }

  def visit(item: WtListItem): Unit = {
    iterate(item)
  }

  def visit(p: EngPage): Unit = {
    iterate(p)
  }

  def visit(text: WtText): Unit = {
    write(text.getContent)
  }

  def visit(w: WtWhitespace): Unit = {
    write(" ")
  }

  def visit(b: WtBold): Unit = {
    iterate(b)
  }

  def visit(i: WtItalics): Unit = {
    iterate(i)
  }

  def visit(cr: WtXmlCharRef): Unit = {
    write(Character.toChars(cr.getCodePoint))
  }

  def visit(er: WtXmlEntityRef): Unit = {
    val ch = er.getResolved
    if (ch == null) {
      write('&')
      write(er.getName)
      write(';')
    }
    else write(ch)
  }

  def visit(wtUrl: WtUrl): Unit = {
    val link = getURL(wtUrl)
    val nifLink = new NifLink(
      begin = None,
      end = None,
      uri = link,
      linkType = NifLinkType.External
    )
    nifLink.begin = Some(context.length)
    write(link)
    nifLink.end = Some(context.length)

    nifParagraph.addLink(nifLink)
  }

  private def getURL(wtUrl: WtUrl):String = {
    if (wtUrl.getProtocol.isEmpty)
      wtUrl.getPath
    else
      wtUrl.getProtocol + ":" + wtUrl.getPath
  }

  def visit(link: WtExternalLink): Unit = {
    var strLink = getURL(link.getTarget)

    val nifLink = new NifLink(
      begin = None,
      end = None,
      uri = strLink,
      linkType = NifLinkType.External
    )
    nifLink.begin = Some(context.length)
    if(link.hasTitle)
      iterate(link.getTitle)
    else{
      write("[" + Integer.toString(extLinkNum) + "]")
      extLinkNum += 1
    }
    nifLink.end = Some(context.length)

    nifParagraph.addLink(nifLink)
  }



  def visit(link: WtInternalLink): Unit = {
    var linkTarget = link.getTarget.getAsString
    var linkType = NifLinkType.Internal
    try
    {
      val destinationTitle = WikiTitle.parse(link.getTarget.getAsString, language)
      if(destinationTitle.namespace.code != 0){ // collect only links into the main namespace
        return
      }
      if (destinationTitle.fragment == null) {
        // for interlanguage links dbpedia extraction framework uses decodedWithNamespace
        //but it should not make a difference because append method also replaces whitespoace with underscore
        //linkTarget = destinationTitle.language.resourceUri.append(destinationTitle.encodedWithNamespace) //same as destinationTitle.resourceIri
        //linkTarget = destinationTitle.language.resourceUri.append(destinationTitle.decodedWithNamespace)
        linkTarget = destinationTitle.resourceIri
      }
      else{
        linkTarget = destinationTitle.resourceIri + "#" + destinationTitle.fragment
      }
      if(destinationTitle.isInterLanguageLink){
        linkType = NifLinkType.InterLanguage
      }else {
        if (destinationTitle.language.dbpediaUri != language.dbpediaUri){
          linkType = NifLinkType.InterWiki
        }else{
          linkType = NifLinkType.Internal
        }
      }
    }
    catch { case _: Throwable  =>  {
      write(link.getPrefix)
      if (link.hasTitle) iterate(link.getTitle)
      else write(link.getTarget.getAsString)
      write(link.getPostfix)
      return
    } }

    val nifLink = new NifLink(
      begin = None,
      end = None,
      uri = linkTarget,
      linkType = linkType
    )
    nifLink.begin = Some(context.length)
    write(link.getPrefix)
    if (link.hasTitle) iterate(link.getTitle)
    else write(link.getTarget.getAsString)
    write(link.getPostfix)
    nifLink.end = Some(context.length)

    nifParagraph.addLink(nifLink)
  }

  def updateCurrentSections(depth:Int):Unit = {
    if(currentSection.size < depth) //first subsection
      currentSection.append(1)
    else {
      //delete last entries depending on the depth difference to the last section
      val del = currentSection.size - depth +1
      val zw = currentSection(currentSection.size - del)
      currentSection.remove(currentSection.size - del, del)
      //if its just another section of the same level -> add one
      if(currentSection.size == depth-1)
        currentSection.append(zw+1)
    }
  }

  def createNifSection():NifSection = {
    val section = new NifSection(
      id="",
      //merge section numbers separated by a dot
      ref = currentSection.map(n => "." + n.toString).foldRight("")(_+_).substring(1),
      //previous section (if on same depth level
      prev = currentSection.last match{
        case x:Int if x > 1 => tocMap.lastOption
        case _ => None
      },
      next = None,
      //super section
      top = tocMap.find(x => currentSection.size > 1 && x.ref == currentSection.slice(0, currentSection.size-1).map(n => "." + n.toString).foldRight("")(_+_).substring(1)),
      sub = None,
      begin = None,
      end = None,
      beginTitle = None,
      endTitle = None,
      paragraphs = ListBuffer()
    )
    section.top match{
      case Some(s) => s.sub = Option(s.sub.getOrElse(section)) // only when sub is not set, then set it otherwise the sub points to the last subsection and not the first
      case None => None
    }
    section.prev match{
      case Some(s) => s.next = Option(section)
      case None => None
    }
    section
  }

  def visit(s: WtSection): Unit = {
    if(nifSection.begin.get != context.length){
      context ++= "\n\n"
      nifSection.end = Some(context.length)
      nifParagraph.end = Some(context.length)

      //new section
      updateCurrentSections(s.getLevel - 1)
      nifSection = createNifSection()
      nifSection.begin = Some(context.length)
      tocMap.append(nifSection)
      //closeParagraphAndStartNew()
      nifParagraph = new NifParagraph(begin = Some(0),end = Some(0),links = ListBuffer()) //dummy paragraph (all lnks in header will be put in dummy)
      nifSection.beginTitle = Some(context.length)
      iterate(s.getHeading)
      nifSection.endTitle = Some(context.length)
      nifSection.id = context.substring(nifSection.beginTitle.get, nifSection.endTitle.get)

      closeParagraphAndStartNew()
    }

    iterate(s.getBody)

/*
    //println("section: " + s.getLevel.toString)
    if(context.nonEmpty)
      context ++= "\n\n"
    if(nifSection.id == "abstract"){
      nifSection.end = Some(context.length)
    }

    updateCurrentSections(s.getLevel - 1)
    val section = createNifSection(s.getHeading.toString)
    tocMap.append(section)
    nifSection = section

    section.begin = Some(context.length)
    iterate(s)
    section.end = Some(context.length)
    */
  }

  private def closeParagraphAndStartNew() = {
      nifParagraph.end = Some(context.length)
      nifParagraph = new NifParagraph(
        begin = Some(context.length),
        end = None,
        links = ListBuffer()
      )
      nifSection.addParagraph(nifParagraph)
  }

  def visit(p: WtParagraph): Unit = {
    //check if we should start a new paragraph
    if(nifParagraph.begin.get != context.length){
      //make a new paragraph
      context ++= "\n"
      closeParagraphAndStartNew()
    }
    iterate(p)

/*
    if(context.nonEmpty)
      context ++= "\n"

    val newP = new NifParagraph(
      begin = None,
      end = None,
      links = ListBuffer()
    )
    nifSection.foreach(_.addParagraph(newP))

    nifParagraph = Some(newP)
    newP.begin = Some(context.length)
    iterate(p)
    newP.end = Some(context.length)
    */
  }






  def visit(n: WtImageLink): Unit = {}
  def visit(n: WtIllegalCodePoint): Unit = {}
  def visit(n: WtTemplate): Unit = {}
  def visit(n: WtTemplateArgument): Unit = {}
  def visit(n: WtTemplateParameter): Unit = {}
  def visit(n: WtTagExtension): Unit = {}
  def visit(n: WtPageSwitch): Unit = {}
  def visit(hr: WtHorizontalRule): Unit = {}
  def visit(n: WtTable): Unit = {}//no table
  def visit(e: WtXmlEndTag): Unit = {}
  def visit(e: WtXmlStartTag): Unit = {}
  def visit(n: WtXmlComment): Unit = {}
  def visit(n: WtXmlAttribute): Unit = {}
  //def visit(n: WtSemiPre): Unit = {}


  def visit(e: WtXmlElement): Unit = {
    //pro iteration: e.g. https://memory-alpha.fandom.com/wiki/Kai at DS9 template
    // https://harrypotter.fandom.com/wiki/Harry_Potter everything in xml element
    //contra iteration: infobox xml on many pages such as https://tardis.fandom.com/wiki/First_Doctor
    //iterate(e.getBody)
  }


  private def write(s: String): Unit = {
    if (s.isEmpty) return
    if(context.isEmpty){
      context ++= s.replace("\n", "").replaceAll(" +", " ").replaceAll("^\\s+", "")
    }else{
      if(context.last == ' '){
        context ++= s.replace("\n", "").replaceAll(" +", " ").replaceAll("^\\s+", "")
      }else{
        context ++= s.replace("\n", "").replaceAll(" +", " ")
      }
    }
  }

  private def write(cs: Array[Char]): Unit = {
    write(String.valueOf(cs))
  }

  private def write(ch: Char): Unit = {
    write(String.valueOf(ch))
  }


}