package org.dbpedia.extraction.hearst;

import java.util.regex.Pattern;

public class CustomPattern {

    private String pid;
    private String regex;
    private String type;
    private Pattern pattern;
    private String preCondition;
    private Boolean excludePronouns;
    private String firstKeyWord;
    private String secondKeyWord;
    private Boolean instanceFirst;

    private String surrounderSymbols = "[\\u0027\\u2018\\u2019\\u201A\\u201B\\u201C\\u201D\\u201E\\u201F\\u0022]?";
    private String endSymbols = "[\"\\u0026\\u0027\\u2018\\u2019\\u201A\\u201B\\u201C\\u201D\\u201E\\u201F\\u00A9\\u00AE]?";	//includes surrounderSymbols as well!
    private String prefix = "(\\p{L}|\\d)" + endSymbols;
    private String suffix = surrounderSymbols + "(\\p{L}|\\d)";

    public CustomPattern(String pid, String regex, String type, Boolean instanceFirst) {
        this.pid = pid;
        this.regex = regex;
        this.type = type;
        this.instanceFirst = instanceFirst;

        //Configure the Prefix and suffix of the regex
        if (type.equals("compact") || type.equals("split")) {
            this.pattern = Pattern.compile(prefix + regex + suffix);
        }

        if (type.equals("split_noPrefix")) {
            this.pattern = Pattern.compile("(?>" + regex + suffix + ")");
        }

        if (type.equals("split_noSuffix")) {
            this.pattern = Pattern.compile("(?>" + prefix + regex + ")");
        }
    }

    public CustomPattern(String pid, String regex, String type, String preCond, Boolean instanceFirst) {
        this.pid = pid;
        this.regex = regex;
        this.type = type;
        this.instanceFirst = instanceFirst;

        //Configure the Prefix and suffix of the regex
        if (type.equals("compact") || type.equals("split")) {
            this.pattern = Pattern.compile(prefix + regex + suffix);
        }

        if (type.equals("split_noPrefix")) {
            this.pattern = Pattern.compile("(?>" + regex + suffix + ")");
        }

        if (type.equals("split_noSuffix")) {
            this.pattern = Pattern.compile("(?>" + prefix + regex + ")");
        }

        this.preCondition = preCond;
    }

    public CustomPattern(String pid, String regex, String type, String preCond, String fkw, String skw, Boolean instanceFirst) {
        this.pid = pid;
        this.regex = regex;
        this.type = type;
        this.firstKeyWord = fkw;
        this.secondKeyWord = skw;
        this.instanceFirst = instanceFirst;

        //Configure the Prefix and suffix of the regex
        if (type.equals("compact") || type.equals("split")) {
            this.pattern = Pattern.compile(prefix + regex + suffix);
        }

        if (type.equals("split_noPrefix")) {
            this.pattern = Pattern.compile("(?>" + regex + suffix + ")");
        }

        if (type.equals("split_noSuffix")) {
            this.pattern = Pattern.compile("(?>" + prefix + regex + ")");
        }

        this.preCondition = preCond;
    }

    public Pattern getPattern() {
        return pattern;
    }

    public String getType() {
        return type;
    }

    public String getFirstKeyWord() {
        return firstKeyWord;
    }

    public String getSecondKeyWord() {
        return secondKeyWord;
    }

    public Boolean getInstanceFirst() {
        return instanceFirst;
    }
    
    
    
    
}
