package org.dbpedia.extraction.nif;

import de.fau.cs.osr.ptk.common.AstVisitor;
import de.fau.cs.osr.utils.StringTools;
import org.sweble.wikitext.engine.PageTitle;
import org.sweble.wikitext.engine.config.WikiConfig;
import org.sweble.wikitext.engine.nodes.EngPage;
import org.sweble.wikitext.parser.nodes.*;
import org.sweble.wikitext.parser.parser.LinkTargetException;

import java.util.LinkedList;
import java.util.regex.Pattern;

public class TextConvert
        extends AstVisitor<WtNode>
{
    private final WikiConfig config;
    private StringBuilder line;
    private boolean shouldWrite=true;

    public TextConvert(WikiConfig config)
    {
        this.config = config;
    }

    protected WtNode before(WtNode node)
    {
        this.line = new StringBuilder();
        return (WtNode)super.before(node);
    }

    protected Object after(WtNode node, Object result)
    {
        return this.line.toString().replaceAll("\\s+", " ").trim();
    }


    public void visit(WtNode n){write(n.getNodeName());}
    public void visit(WtNodeList n)
    {
        iterate(n);
    }
    public void visit(WtUnorderedList e)
    {
        iterate(e);
    }
    public void visit(WtOrderedList e)
    {
        iterate(e);
    }
    public void visit(WtListItem item){iterate(item);}
    public void visit(EngPage p) { iterate(p);}
    public void visit(WtText text){write(text.getContent());}
    public void visit(WtWhitespace w){write(" ");}
    public void visit(WtBold b){iterate(b);}
    public void visit(WtItalics i) {iterate(i);}
    public void visit(WtXmlCharRef cr)
    {
        write(Character.toChars(cr.getCodePoint()));
    }

    public void visit(WtXmlEntityRef er)
    {
        String ch = er.getResolved();
        if (ch == null)
        {
            write('&');
            write(er.getName());
            write(';');
        }
        else
        {
            write(ch);
        }
    }

    public void visit(WtUrl wtUrl)
    {
        if (!wtUrl.getProtocol().isEmpty())
        {
            write(wtUrl.getProtocol());
            write(':');
        }
        write(wtUrl.getPath());
    }

    public void visit(WtInternalLink link)
    {
        try
        {
            if (link.getTarget().isResolved())
            {
                PageTitle page = PageTitle.make(this.config, link.getTarget().getAsString());
                if (page.getNamespace().equals(this.config.getNamespace("Category"))) {
                    return;
                }
            }
        }
        catch (LinkTargetException localLinkTargetException) {}
        write(link.getPrefix());
        if (!link.hasTitle()) {
            iterate(link.getTarget());
        } else {
            iterate(link.getTitle());
        }
        write(link.getPostfix());
    }

    public void visit(WtSection s){ shouldWrite = false; }

    public void visit(WtParagraph p){iterate(p);}
    public void visit(WtHorizontalRule hr){ }
    public void visit(WtXmlElement e) {}
    public void visit(WtImageLink n) {}
    public void visit(WtIllegalCodePoint n) {}
    public void visit(WtXmlComment n) {}
    public void visit(WtTemplate n) {}
    public void visit(WtTemplateArgument n) {}
    public void visit(WtTemplateParameter n) {}
    public void visit(WtTagExtension n) {}
    public void visit(WtPageSwitch n) {}

    private void write(String s)
    {
        if(!shouldWrite)
            return;
        if (s.isEmpty()) {
            return;
        }

        line.append(s); //.replace("\n", " ")
    }

    private void write(char[] cs)
    {
        write(String.valueOf(cs));
    }

    private void write(char ch)
    {
        write(String.valueOf(ch));
    }

    private void write(int num)
    {
        write(String.valueOf(num));
    }
}

