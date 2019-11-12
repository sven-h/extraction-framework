package org.dbpedia.extraction.hearst;

import edu.stanford.nlp.ling.HasWord;
import edu.stanford.nlp.ling.SentenceUtils;
import edu.stanford.nlp.ling.TaggedWord;
import edu.stanford.nlp.pipeline.CoreNLPProtos.Sentence;

import edu.stanford.nlp.tagger.maxent.MaxentTagger;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author shertlin
 */
public class ExtractHearstPatterns {
    
    private static int maxNpSize =  4;

    private static MaxentTagger tagger = new MaxentTagger("edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger");

    private static String separatorSymbols  = 	"[\\u002D\\u2010\\u2011\\u2012\\u2013\\u2014\\u2015\\u2043]?"; 
    private static String surrounderSymbols = "[\\u0027\\u2018\\u2019\\u201A\\u201B\\u201C\\u201D\\u201E\\u201F\\u0022]?";
    private static String endSymbols	    =	"[\"\\u0026\\u0027\\u2018\\u2019\\u201A\\u201B\\u201C\\u201D\\u201E\\u201F\\u00A9\\u00AE]?";    
    
    private static String npPlaceholder = "("+surrounderSymbols+""				//Quotation mark could be in front
			+ "(\\p{L}++|\\d++\\p{L}++)"						//Word can start with letters or digits but must contain letters
			+ "("+separatorSymbols+"(\\p{L}++|\\d++))?"				//Can be separated by a hyphen
			+ endSymbols+"\\s)"							//Can be followed by quotation mark
			+ "{1,4}";								//NP can consist of up to 4 words
	
    private static String npPlaceholderAdjMost = "("+surrounderSymbols+""		//Quotation mark could be in front
                    + "(\\p{L}++|\\d++\\p{L}++)"					//Word can start with letters or digits but must contain letters
                    + "("+separatorSymbols+"(\\p{L}++|\\d++))?"				//Can be separated by a hyphen
                    + endSymbols+"\\s)"							//Can be followed by quotation mark
                    + "{2,5}";
    
    private static List<CustomPattern> allPatterns = generatePatterns();
    private static List<CustomPattern> generatePatterns() {
        List<CustomPattern> allPatterns = new ArrayList<CustomPattern>();
        allPatterns.add(new CustomPattern("p8a", "\\,?\\sis\\san?\\s", "compact", "is\\sa", true));
        allPatterns.add(new CustomPattern("p8b", "\\,?\\swas\\san?\\s", "compact", "was\\sa", true));
        /*
        allPatterns.add(new CustomPattern("p3a", "\\,?\\sincluding\\s", "compact", "including", false));
        allPatterns.add(new CustomPattern("p5", "\\,?\\ssuch\\sas\\s", "compact", "such\\sas", false));
        allPatterns.add(new CustomPattern("p1", "\\,?\\sand\\sother\\s", "compact", "and\\sother", true));
        allPatterns.add(new CustomPattern("p4", "\\,?\\sor\\sother\\s", "compact", "or\\sother", true));
        allPatterns.add(new CustomPattern("p2", "\\,?\\sespecially\\s", "compact", "especially", false));
        allPatterns.add(new CustomPattern("p8c", "\\,?\\sare\\san?\\s", "compact", "are\\sa", true));		
        allPatterns.add(new CustomPattern("p34", "\\stypes\\s", "compact", "types", false));
        allPatterns.add(new CustomPattern("p25", "\\,?\\sexcept\\s", "compact", "except", false));
        allPatterns.add(new CustomPattern("p23d", "\\,?\\sparticularly\\s", "compact", "particularly", false));
        allPatterns.add(new CustomPattern("p20a", "\\sis\\sthe\\s\\w+est\\s", "compact", "is\\sthe", true));
        allPatterns.add(new CustomPattern("p43", "\\,?\\ssort\\sof\\s", "compact", "sort\\sof", true));
        allPatterns.add(new CustomPattern("p26", "\\,?\\sother\\sthan\\s", "compact", "other\\sthan", false));

        allPatterns.add(new CustomPattern("p21a", "\\p{L}+est\\s"+npPlaceholder+"is\\s", "split_noPrefix", "est\\s", "est", "is", false));
        allPatterns.add(new CustomPattern("p21b", "\\p{L}+est\\s"+npPlaceholder+"are\\s", "split_noPrefix", "est\\s", "est", "are", false));
        allPatterns.add(new CustomPattern("p21c", "\\s(M|m)ost\\s"+npPlaceholderAdjMost+"is\\s", "split_noPrefix", "most\\s", "most", "is", false));
        allPatterns.add(new CustomPattern("p21d", "\\s(M|m)ost\\s"+npPlaceholderAdjMost+"are\\s", "split_noPrefix", null, "most", "are", false));

        allPatterns.add(new CustomPattern("p23b", "\\,?\\smostly\\s", "compact", "mostly", false));
        allPatterns.add(new CustomPattern("p23a", "\\,?\\smainly\\s", "compact", "mainly", false));
        allPatterns.add(new CustomPattern("p12a", "\\,\\sone\\sof\\sthe\\s", "compact", "one\\sof\\sthe", true));
        allPatterns.add(new CustomPattern("p20c", "\\sis\\sthe\\smost\\s\\w+\\s", "compact", true));
        allPatterns.add(new CustomPattern("p8d", "\\,?\\swere\\san?\\s", "compact", "were\\sa", true));
        allPatterns.add(new CustomPattern("p6", "\\,?\\sand\\sany\\sother\\s", "compact", "and\\sany\\sother", true));
        allPatterns.add(new CustomPattern("p15a", "\\sexamples\\sof\\s", "compact", "examples\\sof", true));
        allPatterns.add(new CustomPattern("p27a", "\\,?\\se\\.g\\.\\s", "compact", "e\\.g\\.", false));
        allPatterns.add(new CustomPattern("p27b", "\\,?\\si\\.e\\.\\s", "compact", "i\\.e\\.", false));
        allPatterns.add(new CustomPattern("p16", "\\,?\\sfor\\sexample\\s", "compact", "for\\sexample", false));
        allPatterns.add(new CustomPattern("p24", "\\,?\\sin\\sparticular\\s", "compact", "in\\sparticular", false));
        allPatterns.add(new CustomPattern("p20b", "\\sare\\sthe\\s\\w+est\\s", "compact", "are\\sthe", true));
        allPatterns.add(new CustomPattern("p20d", "\\sare\\sthe\\smost\\s\\w+\\s", "compact", true));
        allPatterns.add(new CustomPattern("p23c", "\\,?\\snotably\\s", "compact", "notably", false));
        allPatterns.add(new CustomPattern("p39", "\\,?\\samong\\sthem\\s", "compact", "\\samong\\sthem", false));
        allPatterns.add(new CustomPattern("p38", "\\scompared\\sto\\sother\\s", "compact", "compared\\sto", true));
        allPatterns.add(new CustomPattern("p11", "\\,?\\slike\\sother\\s", "compact", "like\\sother", true));
        allPatterns.add(new CustomPattern("p7", "\\,?\\sand\\ssome\\sother\\s", "compact", "and\\some\\sother", true));
        allPatterns.add(new CustomPattern("p23e", "\\,?\\sprincipally\\s", "compact", "principally", false));		
        allPatterns.add(new CustomPattern("p15b", "\\sis\\san\\sexample\\sof\\s", "compact", "is\\san\\sexample\\sof", true));
        allPatterns.add(new CustomPattern("p22a", "\\,?\\swhich\\sis\\scalled\\s", "compact", "which\\sis\\scalled", false));		
        allPatterns.add(new CustomPattern("p28a", "\\,?\\sa\\skind\\sof\\s", "compact", "a\\skind\\sof", true));
        allPatterns.add(new CustomPattern("p12c", "\\,\\sone\\sof\\sthose\\s", "compact", "one\\sof\\sthose", true));
        allPatterns.add(new CustomPattern("p29a", "\\,?\\swhich\\slooks?\\slike\\s", "compact", "which\\slooks?\\slike", false));
        allPatterns.add(new CustomPattern("p28c", "\\,?\\sa\\sform\\sof\\s", "compact", "a\\sform\\sof", true));
        allPatterns.add(new CustomPattern("p30b", "\\,?\\swhich\\sis\\ssimilar\\sto\\s", "compact", "which\\sis\\ssimilar\\sto", false));
        allPatterns.add(new CustomPattern("p12b", "\\,\\sone\\sof\\sthese\\s", "compact", "one\\sof\\sthese", true));
        allPatterns.add(new CustomPattern("p29c", "\\,?\\swhich\\ssounds?\\slike\\s", "compact", "which\\ssounds?\\slike", false));
        allPatterns.add(new CustomPattern("p28d", "\\,?\\sforms\\sof\\s", "compact", "forms\\sof", true));
        allPatterns.add(new CustomPattern("p30a", "\\,?\\swhich\\sare\\ssimilar\\sto\\s", "compact", "which\\sare\\ssimilar\\sto", false));
        allPatterns.add(new CustomPattern("p22b", "\\,?\\swhich\\sis\\snamed\\s", "compact", "which\\sis\\snamed", false));
        allPatterns.add(new CustomPattern("p42", "\\,?\\sor\\sthe\\smany\\s", "compact", "or\\sthe\\smany", true));
        allPatterns.add(new CustomPattern("p31a", "\\,?\\sexample\\sof\\sthis\\sis\\s", "compact", "example\\sof\\sthis\\sis", false));
        allPatterns.add(new CustomPattern("p28b", "\\,?\\skinds\\sof\\s", "compact", "kinds\\sof", true));	
        allPatterns.add(new CustomPattern("p31b", "\\,?\\sexamples\\sof\\sthis\\sare\\s", "compact", "examples\\sof\\sthis\\sare", false));

        allPatterns.add(new CustomPattern("p10", "(S|s)uch\\s"+npPlaceholder+"as\\s", "split_noPrefix", "(S|s)uch\\s", "such", "as", false));
        allPatterns.add(new CustomPattern("p13", "(E|e)xample\\sof\\s"+npPlaceholder+"is\\s", "split_noPrefix", "example\\sof", "example of", "is", false));
        allPatterns.add(new CustomPattern("p14", "(E|e)xamples\\sof\\s"+npPlaceholder+"are\\s", "split_noPrefix", null, "examples of", "are", false));
        allPatterns.add(new CustomPattern("p36", "\\swhether\\s"+npPlaceholder+"or\\s", "split", " whether", "whether", "or", false));
        allPatterns.add(new CustomPattern("p37", "(C|c)ompare\\s"+npPlaceholder+"with\\s", "split_noPrefix", "compare\\s", "compare", "with", true));
         */
        return allPatterns;
    }

    
    public static NounPhrase extract(String text, String gold_instance_label) {
        Set<String> gold_instance_tokens = new HashSet(Arrays.asList(gold_instance_label.toLowerCase().split(" ")));
        List<IsaPattern> patterns = extract(text);
        for(IsaPattern p : patterns){
            for(NounPhrase i : p.getInstance()){
                Set<String> instanceTokens = new HashSet(Arrays.asList(i.toString().toLowerCase().split(" ")));
                instanceTokens.retainAll(gold_instance_tokens);
                if(instanceTokens.size() > 0){
                    return p.getClazz().get(0);
                }
            }
        }
        return null;        
    }
    
    public static List<IsaPattern> extract(String s) {
        List<IsaPattern> extractedPatterns = new ArrayList<IsaPattern>();
        
        List<String> sentencesClean = splitSentences(s);
        for (String sentence : sentencesClean) {
            sentence = preprocessSentence(sentence);
            
            for (CustomPattern customPattern : allPatterns) {
                Matcher patternMatcher = customPattern.getPattern().matcher(sentence);
                while (patternMatcher.find()) {
                    
                    String extractedPattern = patternMatcher.group();
                    int onset = patternMatcher.start();
                    int offset = patternMatcher.end();
                    
                    // Compact Patterns still contain an Indicator for the leading and the following Nounphrase
                    // This indicator has to be removed
                    if (customPattern.getType().equals("compact")){
                        onset++;
                        offset--;
                    }
                    
                    // Check if a leading pronoun can be excluded
                    String pronounFront = sentence.substring(0, onset);
                    int lastWhitespace = pronounFront.lastIndexOf(" ");
                    if (lastWhitespace != -1){
                        pronounFront = pronounFront.substring(lastWhitespace+1).toLowerCase();
                    }
                    
                    String pronounBack = sentence.substring(offset);
                    int firstWhitespace = pronounBack.indexOf(" ");
                    if (firstWhitespace != -1){
                        pronounBack = pronounBack.substring(0, firstWhitespace).toLowerCase();
                    }
                    
                    if(allExclusions.contains(pronounFront.toLowerCase()) || allExclusions.contains(pronounBack.toLowerCase())){
                        continue;
                    }
                    
                    ArrayList<NounPhrase> currentNPsBeforePattern = new ArrayList<>();
                    ArrayList<NounPhrase> currentNPsAfterPattern = new ArrayList<>();
                    try{
                        List<TaggedWord> fullTaggedList = tagger.tagSentence(SentenceUtils.toWordList(sentence.split(" ")));

                        for (TaggedWord tw : fullTaggedList) {
                            if (tw.word().length() < tw.word().replaceAll("(?<=s)[\\u201A\\u201C\\u201D\\u201E\\u201F\\u0022]", "").length()
                                    || tw.word().length() < tw.word().replaceAll("[\\u201A\\u201C\\u201D\\u201E\\u201F\\u0022](?=s)", "").length()) {
                                tw.setTag("JJ");
                            }
                        }
                        
                        List<TaggedWord> taggedWordsBeforePattern;
                        List<TaggedWord> taggedWordsAfterPattern;
                        if (customPattern.getType().equals("compact")) {
                            taggedWordsBeforePattern = getWordListSubset(0, onset + 1, fullTaggedList);
                            taggedWordsAfterPattern = getWordListSubset(offset, sentence.length(), fullTaggedList);
                        } else {
                            taggedWordsBeforePattern = getWordlistBeforeSplittedPattern(customPattern, sentence, onset, fullTaggedList);
                            taggedWordsAfterPattern = getWordlistAfterSplittedPattern(customPattern, sentence, onset, offset, fullTaggedList);
                        }

                        Collections.reverse(taggedWordsBeforePattern);
                        findNextNounPhraseReverse(0, taggedWordsBeforePattern, currentNPsBeforePattern);
                        findNextNounPhrase(0, taggedWordsAfterPattern, currentNPsAfterPattern);

                        if (currentNPsAfterPattern.isEmpty() || currentNPsBeforePattern.isEmpty()){
                            continue;
                        }

                    }catch (StringIndexOutOfBoundsException e){ }

                    
                    if (customPattern.getInstanceFirst()){
                        extractedPatterns.add(new IsaPattern(currentNPsBeforePattern, currentNPsAfterPattern));
                    }
                    else {
                        extractedPatterns.add(new IsaPattern(currentNPsAfterPattern, currentNPsBeforePattern));
                    }
                }
            }
        }
        return extractedPatterns;
    }

    
    private static String[] abbreviations = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
        "Adj", "Adm", "Adv", "Asst", "Bart", "Bldg", "Brig", "Bros", "Capt", "Cmdr", "Col", "Comdr", "Con", "Corp", "Cpl", "DR", "Dr", "Drs", "Ens", "Fig", "FIG", "fig", "Gen", "Gov", "Hon", "Hr", "Hosp", "Insp", "Lt", "MM",
        "MR", "MRS", "MS", "Maj", "Messrs", "Mlle", "Mme", "Mr", "Mrs", "Ms", "Msgr", "Op", "Ord", "Pat", "Pfc", "Ph", "Prof", "Pvt", "Rep", "Reps", "Res", "Rev", "Rt", "Sen", "Sens", "Sfc", "Sgt", "Sr", "St",
        "Supt", "Surg", "v", "vs", "U.S", "u.s", "U.K", "u.k", "i.e", "rev", "e.g", "No", "Nos", "Art", "Nr", "pp"};
    private static Pattern splitRegex = Pattern.compile("(?<=[\\!\\.\\?]" + surrounderSymbols + ")\\s(?=" + surrounderSymbols + "\\p{Lu})");

    private static List<String> splitSentences(String text) {
        String[] sentencesRaw = splitRegex.split(text);

        Boolean connectWithLatterPart = false;
        List<String> sentencesClean = new ArrayList<String>();
        sentencesClean.add(sentencesRaw[0]);
        for (int z = 1; z < sentencesRaw.length; z++) {
            for (String abb : abbreviations) {
                if (sentencesClean.get(sentencesClean.size() - 1).endsWith(abb + ".")) {
                    connectWithLatterPart = true;
                    break;
                }
            }
            if (connectWithLatterPart) {
                sentencesClean.set(sentencesClean.size() - 1, sentencesClean.get(sentencesClean.size() - 1) + " " + sentencesRaw[z]);
            } else {
                sentencesClean.add(sentencesRaw[z]);
            }
            connectWithLatterPart = false;
        }

        // If no valid sentence is found, the line itself is analyzed
        if (sentencesClean.size() == 0) {
            sentencesClean.add(text);
        }

        return sentencesClean;
    }

    private static Pattern multipleWhitespacePatternTwo = Pattern.compile("[Â \\t\\p{Zs}\\n\\cK\\f\\r\\x85\\x{2028}\\x{2029}]+");
    private static Pattern multipleWhitespacePattern = Pattern.compile("\\s+");
    private static Pattern quotationMarkPattern = Pattern.compile("(?<!s)[\\u201A\\u201C\\u201D\\u201E\\u201F\\u0022](?!s)");
    private static Pattern parenthesisPattern = Pattern.compile("\\(.*?\\)|\\[.*?\\]");

    private static String preprocessSentence(String sentence) {
        // Help the Pos-Tagger with Apostrophies and QuotationMarks; Replace as many as possible, without losing the meaning;
        
        sentence = multipleWhitespacePatternTwo.matcher(sentence).replaceAll(" ");
        sentence = parenthesisPattern.matcher(sentence).replaceAll(""); // replace all text between parenthesis
        sentence = multipleWhitespacePattern.matcher(sentence).replaceAll(" ");
        sentence = replaceVerbApostrophies(sentence);
        sentence = quotationMarkPattern.matcher(sentence).replaceAll(" ");
        sentence = sentence.trim();
        return sentence;
    }

    /**
     * Transforms the most common usages of auxiliary-verb apostrophies of the
     * sentence, into the regular form, with the apostrophy replaced; 4 Types of
     * apostrophies are considered
     *
     * @param sentence
     * @return sentence
     */
    public static String replaceVerbApostrophies(String sentence) {
        if (sentence.contains("'") | sentence.contains("’") | sentence.contains("‘") | sentence.contains("‛")) {
            //Auxiliary Verb abbreviations
            sentence = sentence.replaceAll("(?i)[\\u0027\\u2018\\u2019\\u201B]d\\s", " would ");
            sentence = sentence.replaceAll("(?i)[\\u0027\\u2018\\u2019\\u201B]re\\s", " are ");
            sentence = sentence.replaceAll("(?i)[\\u0027\\u2018\\u2019\\u201B]ve\\s", " have ");
            sentence = sentence.replaceAll("(?i)[\\u0027\\u2018\\u2019\\u201B]ll\\s", " will ");
            sentence = sentence.replaceAll("(?i)i[\\u0027\\u2018\\u2019\\u201B]m\\s", "I am ");

            //Auxiliary Verb 's
            sentence = sentence.replaceAll("(?i)he[\\u0027\\u2018\\u2019\\u201B]s\\s", "he is ");
            sentence = sentence.replaceAll("(?i)she[\\u0027\\u2018\\u2019\\u201B]s\\s", "she is ");
            sentence = sentence.replaceAll("(?i)it[\\u0027\\u2018\\u2019\\u201B]s\\s", "it is ");
            sentence = sentence.replaceAll("(?i)that[\\u0027\\u2018\\u2019\\u201B]s\\s", "he is ");
            sentence = sentence.replaceAll("(?i)where[\\u0027\\u2018\\u2019\\u201B]s\\s", "she is ");
            sentence = sentence.replaceAll("(?i)who[\\u0027\\u2018\\u2019\\u201B]s\\s", "it is ");
            sentence = sentence.replaceAll("(?i)what[\\u0027\\u2018\\u2019\\u201B]s\\s", "what is ");
            sentence = sentence.replaceAll("(?i)when[\\u0027\\u2018\\u2019\\u201B]s\\s", "when is ");
            sentence = sentence.replaceAll("(?i)why[\\u0027\\u2018\\u2019\\u201B]s\\s", "why is ");
            sentence = sentence.replaceAll("(?i)how[\\u0027\\u2018\\u2019\\u201B]s\\s", "how is ");
            sentence = sentence.replaceAll("(?i)here[\\u0027\\u2018\\u2019\\u201B]s\\s", "here is ");
            sentence = sentence.replaceAll("(?i)there[\\u0027\\u2018\\u2019\\u201B]s\\s", "there is ");

            //Negations
            sentence = sentence.replaceAll("(?i)isn[\\u0027\\u2018\\u2019\\u201B]t\\s", "is not ");
            sentence = sentence.replaceAll("(?i)aren[\\u0027\\u2018\\u2019\\u201B]t\\s", "are not ");
            sentence = sentence.replaceAll("(?i)don[\\u0027\\u2018\\u2019\\u201B]t\\s", "do not ");
            sentence = sentence.replaceAll("(?i)doesn[\\u0027\\u2018\\u2019\\u201B]t\\s", "does not ");
            sentence = sentence.replaceAll("(?i)can[\\u0027\\u2018\\u2019\\u201B]t\\s", "can not ");
            sentence = sentence.replaceAll("(?i)couldn[\\u0027\\u2018\\u2019\\u201B]t\\s", "could not ");
            sentence = sentence.replaceAll("(?i)shouldn[\\u0027\\u2018\\u2019\\u201B]t\\s", "should not ");
            sentence = sentence.replaceAll("(?i)won[\\u0027\\u2018\\u2019\\u201B]t\\s", " will not ");
            sentence = sentence.replaceAll("(?i)wouldn[\\u0027\\u2018\\u2019\\u201B]t\\s", "would not ");
            sentence = sentence.replaceAll("(?i)haven[\\u0027\\u2018\\u2019\\u201B]t\\s", "have not ");
        }
        return sentence;
    }
    
    
    /**
    * Depending on the type of custPat and the Length of its two parts, the onset and offset of the first NounPhrase-Search-Area
    * @param custPat
    * @param sentence
    * @param onset (Of the pattern inside the sentence)
    * @param offset (Of the pattern inside the sentence)
    * @param tw
    * @return
    */
    public static List<TaggedWord> getWordlistBeforeSplittedPattern(CustomPattern custPat, String sentence, int onset, List<TaggedWord> tw) {
        //Beginnt nach First KeyWord und endet vor second KeyWord; 
        if (custPat.getType().equals("split_noPrefix")) {
            return getWordListSubset(onset + custPat.getFirstKeyWord().length() + 1, sentence.toLowerCase().indexOf(custPat.getSecondKeyWord(), onset), tw);
        }
        //Beginnt vor erstem Keyword
        if (custPat.getType().equals("split")) {
            return getWordListSubset(0, onset, tw);
        }
        //Onset wird "normal gesplitted in diesem Fall" (wie Compact Patterns)
        if (custPat.getType().equals("split_noSuffix")) {
            return getWordListSubset(0, onset, tw);
        }
        return new ArrayList<TaggedWord>();
    }
    
    /**
    * Depending on the type of custPat and the Length of its two parts, the onset and offset of the second NounPhrase-Search-Area
    * @param custPat
    * @param sentence
    * @param onset (Of the pattern inside the sentence)
    * @param offset (Of the pattern inside the sentence)
    * @param tw
    * @return
    */
    public static List<TaggedWord> getWordlistAfterSplittedPattern(CustomPattern custPat, String sentence, int onset, int offset, List<TaggedWord> tw) {
        //Beginnt nach dem zweiten Keyword
        if (custPat.getType().equals("split_noPrefix")) {
            return getWordListSubset(offset, sentence.length(), tw);
        }
        // Beginnt beim Onset und endet mit secondKeyword
        if (custPat.getType().equals("split_noSuffix")) {
            return getWordListSubset(onset, sentence.indexOf(custPat.getSecondKeyWord(), onset), tw);

        }
        //Beginnt nach dem ersten Keyword
        if (custPat.getType().equals("split")) {
            return getWordListSubset(onset + custPat.getFirstKeyWord().length(), sentence.length(), tw);
        }
        return new ArrayList<TaggedWord>();
    }
    
    /**
     *
     * @param wordOffset This word offset points to a word in the sentence list
     * and describes the starting point for the NounPhrase search
     * @param sentence
     * @param resultNPs
     * @return
     */
    public static ArrayList<NounPhrase> findNextNounPhrase(int wordOffset, List<TaggedWord> sentence, ArrayList<NounPhrase> resultNPs) {
        NounPhrase currentNP = new NounPhrase(maxNpSize);
        int postOffset = 0;
        for (int i = wordOffset; i < sentence.size(); i++) {
            if (!sentence.get(i).tag().equals("DT") && !sentence.get(i).tag().startsWith("NN") && !sentence.get(i).tag().startsWith("JJ") && !sentence.get(i).tag().equals("VBN") && !sentence.get(i).word().toLowerCase().equals("and") && !sentence.get(i).word().toLowerCase().equals("or") && !sentence.get(i).word().toLowerCase().equals("&") && resultNPs.size() > 0) {
                return resultNPs;
            }

            if (sentence.get(i).tag().startsWith("NN")) {
                currentNP.setNPCore(sentence.get(i));
                findPreMod(i, sentence, currentNP);
                if (sentence.get(i).word().endsWith(",")) {
                    resultNPs.add(cleanNounPhrase(currentNP, false));
                    findNextNounPhrase(i + 1, sentence, resultNPs);
                    return resultNPs;
                }
                postOffset = findPostMod(i, sentence, currentNP);
                if (postOffset != -1) {
                    resultNPs.add(cleanNounPhrase(currentNP, false));
                    findNextNounPhrase(postOffset + 1, sentence, resultNPs);
                    return resultNPs;
                } else {
                    resultNPs.add(cleanNounPhrase(currentNP, false));
                    return resultNPs;
                }
            }
        }
        return resultNPs;
    }
    
    /**
     * Searches for potential pre-modifying words of the NounPhrase;
     *
     * @param nnOffset Is a pointer on the last word analyzed (which has to be a
     * valid Core-NN)
     * @param sentence
     * @param currentNP
     */
    public static void findPreMod(int nnOffset, List<TaggedWord> sentence, NounPhrase currentNP) {
        for (int i = nnOffset - 1; i > nnOffset - currentNP.getMaxNPLength() && i >= 0; i--) {
            if ((sentence.get(i).tag().startsWith("JJ") | sentence.get(i).tag().equals("VBN")) && !sentence.get(i).word().endsWith(",")) {
                currentNP.addPreModifier(sentence.get(i));
            } else {
                return;
            }
        }
    }

    /**
     * Searches and stores potential post-modifying words of the NounPhrase, as
     * well as looks for potential coordinations
     *
     * @param nnOffset Is a pointer on the last word analyzed (which has to be a
     * valid Core-NN)
     * @param sentence
     * @param currentNP
     * @return -1, if no potential coordination was found; word-offset of the
     * last analyzed word (will be used to search for next NounPhrase)
     */
    public static int findPostMod(int nnOffset, List<TaggedWord> sentence, NounPhrase currentNP) {
        for (int i = nnOffset + 1; i < nnOffset + currentNP.getMaxNPLength() && i < sentence.size(); i++) {
            if (sentence.get(i).tag().startsWith("JJ") | sentence.get(i).tag().equals("VBN") | sentence.get(i).tag().equals("VBG") | sentence.get(i).tag().startsWith("NN") | sentence.get(i).tag().equals("IN") | sentence.get(i).tag().equals("CD") | sentence.get(i).tag().equals("DT")) {
                currentNP.addPostModifier(sentence.get(i));
            }
            if (sentence.get(i).word().toLowerCase().equals("and") | sentence.get(i).word().toLowerCase().equals("or") | sentence.get(i).word().toLowerCase().equals("&")) {
                return i;
            }
            if (!(sentence.get(i).tag().startsWith("JJ") | sentence.get(i).tag().equals("VBN") | sentence.get(i).tag().equals("VBG") | sentence.get(i).tag().startsWith("NN") | sentence.get(i).tag().equals("IN") | sentence.get(i).tag().equals("CD") | sentence.get(i).tag().equals("DT") | sentence.get(i).word().toLowerCase().equals("and") | sentence.get(i).word().toLowerCase().equals("or") | sentence.get(i).word().toLowerCase().equals("&"))) {
                return -1;
            }
            if (sentence.get(i).word().endsWith(",")) {
                return i;
            }
        }
        if (sentence.size() > nnOffset + currentNP.getMaxNPLength()) {
            if (sentence.get(nnOffset + currentNP.getMaxNPLength()).word().toLowerCase().equals("and") | sentence.get(nnOffset + currentNP.getMaxNPLength()).word().toLowerCase().equals("or") | sentence.get(nnOffset + currentNP.getMaxNPLength()).word().toLowerCase().equals("&")) {
                return nnOffset + currentNP.getMaxNPLength();
            }
        }
        return -1;
    }
    
    public static ArrayList<NounPhrase> findNextNounPhraseReverse(int wordOffset, List<TaggedWord> sentence, ArrayList<NounPhrase> resultNPs) {
        int status = 0;
        NounPhrase currentNP = new NounPhrase(maxNpSize);
        for (int i = wordOffset; i < sentence.size(); i++) {
            if (!sentence.get(i).tag().startsWith("NN") && !sentence.get(i).tag().equals("VBG") && !sentence.get(i).tag().equals("IN") && !sentence.get(i).tag().equals("CD") && !sentence.get(i).tag().equals("DT") && resultNPs.size() > 0) {
                return resultNPs;
            }

            if (sentence.get(i).tag().startsWith("NN")) {
                currentNP.setNPCore(sentence.get(i));
                if (sentence.get(i).word().endsWith(",")) {
                    status = findPreModReverse(i, sentence, currentNP);
                    if (status == -2) {
                        findNextNounPhraseReverse(i + 1, sentence, resultNPs);
                        return resultNPs;
                    } else if (status > 0) {
                        resultNPs.add(cleanNounPhrase(currentNP, true));
                        findNextNounPhraseReverse(status + 1, sentence, resultNPs);
                        return resultNPs;
                    } else {
                        resultNPs.add(cleanNounPhrase(currentNP, true));
                        return resultNPs;
                    }
                } else {
                    status = findPreModReverse(i, sentence, currentNP);
                    if (status == -2) {
                        findNextNounPhraseReverse(i + 1, sentence, resultNPs);
                        return resultNPs;
                    }
                    //Neue NounPhrase Entdeckt y=>ist neues Offset
                    if (status > 0) {
                        findPostModReverse(i, sentence, currentNP);
                        resultNPs.add(cleanNounPhrase(currentNP, true));
                        findNextNounPhraseReverse(status, sentence, resultNPs);
                        return resultNPs;
                    } else {
                        findPostModReverse(i, sentence, currentNP);
                        resultNPs.add(cleanNounPhrase(currentNP, true));
                        return resultNPs;
                    }
                }
            }
        }
        return resultNPs;
    }
    
    /**
     * In this stage the post-modifier of NounPHrase is reduced until the Last
     * NN is found; The cleaning of NounPhrases with regard to Symbols can be
     * done in the Aggregation step; At this stage the risk is too high, that
     * valuable information gets lost / Algorithms can't be too precise at this
     * stage
     *
     * @param np
     * @param isReverse
     * @return
     */
    public static NounPhrase cleanNounPhrase(NounPhrase np, Boolean isReverse) {
        //If dot is inside a NN, the NN is split: Depending if entity was extracted inFront of or afterPattern
        /*
		String[] npPartHolder = np.getNPCore().word().split("(?<=(\\p{L}|\\d))\\s?[\\.\\?\\!\\(\\)\\{\\}\\[\\]]+\\s?(?=\\p{L})");
		
		if (npPartHolder.length>=2)
		{
			if (isReverse)
			{
				np.NPCore.setWord(npPartHolder[npPartHolder.length-1]);
				np.clearPreMod();
			}
			else
			{
				np.NPCore.setWord(npPartHolder[0]);
				np.clearPostMod();
			}
		}*/

        //Reduce the Post-Modifier or PreModifier until the last NN is found
        try {
            if (np.getPostModifier() != null && !np.getPostModifier().isEmpty()) {
                np.getPostModifier().get(np.getPostModifier().size() - 1).setWord(np.getPostModifier().get(np.getPostModifier().size() - 1).word().trim().replaceAll("(\\.|\\,|\\;|\\:|\\?|\\!)$", ""));
                while (!np.getPostModifier().get(np.getPostModifier().size() - 1).tag().startsWith("NN") && !np.getPostModifier().get(np.getPostModifier().size() - 1).tag().equals("CD")) {
                    np.getPostModifier().remove(np.getPostModifier().size() - 1);
                    if (np.getPostModifier().isEmpty()) {
                        break;
                    }
                }
            }

            if (np.getPreModifier() != null && !np.getPreModifier().isEmpty()) {
                np.getPreModifier().get(np.getPreModifier().size() - 1).setWord(np.getPreModifier().get(np.getPreModifier().size() - 1).word().trim().replaceAll("(\\.|\\,|\\;|\\:|\\?|\\!)$", ""));
                while (!np.getPreModifier().get(0).tag().startsWith("JJ") && !np.getPreModifier().get(0).tag().equals("VBN")) {
                    np.getPreModifier().remove(0);
                    if (np.getPreModifier().isEmpty()) {
                        break;
                    }
                }
            }
        } catch (ArrayIndexOutOfBoundsException e) { }

        //Clean the single word of NounPhrases if there are still symbols in front or behind it; (in case of multiple symbols)
        //Problem with abbreviations or websites => Can be filtered in next steps
        // KOmma removed, because it intereferes with Coordinations; (Not in Aggregation step ;-) )
        //KOmma added - Sven
        np.NPCore.setWord(np.NPCore.word().trim().replaceAll("[\\.\\,\\;\\:\\?\\!\\(\\)\\[\\]\\{\\}]+$", "")); 
        np.NPCore.setWord(np.NPCore.word().trim().replaceAll("^[\\.\\,\\;\\:\\?\\!\\(\\)\\[\\]\\{\\}]+", ""));
        
        return np;
    }
    
    public static int findPreModReverse(int nnOffset, List<TaggedWord> sentence, NounPhrase currentNP) {
        boolean premodFinished = false;
        for (int i = nnOffset + 1; i < nnOffset + currentNP.getMaxNPLength() && i < sentence.size(); i++) {
            if ((sentence.get(i).tag().startsWith("JJ") | sentence.get(i).tag().equals("VBN")) && !sentence.get(i).word().endsWith(",") && !premodFinished) {
                currentNP.addPreModifier(sentence.get(i));
            } else if ((sentence.get(i).tag().equals("VBG") | sentence.get(i).tag().equals("IN") | sentence.get(i).tag().equals("CD") | sentence.get(i).tag().equals("DT"))) {
                //ignore: It might be that this is pat of a postModifier: if another NN is found;
                premodFinished = true;
            } else if (sentence.get(i).tag().startsWith("NN") && !sentence.get(i).word().endsWith(",")) {
                return -2;
            } else if (sentence.get(i).word().endsWith(",")) {
                return i;
            } else if (sentence.get(i).word().toLowerCase().equals("and") | sentence.get(i).word().toLowerCase().equals("or") | sentence.get(i).word().toLowerCase().equals("&")) {
                return i + 1;
            } else {
                return -1;
            }
        }

        if (sentence.size() > nnOffset + currentNP.getMaxNPLength()) {
            if (sentence.get(nnOffset + currentNP.getMaxNPLength()).word().toLowerCase().equals("and") | sentence.get(nnOffset + currentNP.getMaxNPLength()).word().toLowerCase().equals("or") | sentence.get(nnOffset + currentNP.getMaxNPLength()).word().toLowerCase().equals("&")) {
                return nnOffset + currentNP.getMaxNPLength() + 1;
            }
        }
        return -1;
    }
    
    
    public static void findPostModReverse(int nnOffset, List<TaggedWord> sentence, NounPhrase currentNP) {
        for (int i = nnOffset - 1; i > nnOffset - currentNP.getMaxNPLength() && i >= 0; i--) {
            if (sentence.get(i).word().toLowerCase().equals("and") | sentence.get(i).word().toLowerCase().equals("or") | sentence.get(i).word().toLowerCase().equals("&")) {
                return;
            }

            if (sentence.get(i).tag().startsWith("JJ") | sentence.get(i).tag().equals("VBN") | sentence.get(i).tag().equals("VBG") | sentence.get(i).tag().startsWith("NN") | sentence.get(i).tag().equals("IN") | sentence.get(i).tag().equals("CD") | sentence.get(i).tag().equals("DT")) {
                currentNP.addPostModifier(sentence.get(i));
            }

            if (sentence.get(i).word().endsWith(",")) {
                return;
            }
        }
    }

    
    /**
     * The method returns a subList of a tagged-Word List (the entire sentence).
     * The Onset and Offset are pointers in the sentence String, which restrict
     * the retunred List of words;
     *
     * @param onset
     * @param offset
     * @param taggedWords
     * @return
     */
    public static List<TaggedWord> getWordListSubset(int onset, int offset, List<TaggedWord> taggedWords) {
        List<TaggedWord> result = new ArrayList<TaggedWord>();
        int charCounter = 0;
        for (TaggedWord tw : taggedWords) {
            charCounter += tw.word().length();
            if (charCounter >= onset && charCounter <= offset) {
                result.add(tw);
            }
            charCounter++;
        }
        return result;
    }

    
    //Variables and Constants for false-positive detection
    private static Set<String> allExclusions = createAllExclusions();
    private static Set<String> createAllExclusions(){
        Set<String> exclusions = new HashSet<>();
        exclusions.addAll(Arrays.asList("that","this","these","those"));//demonstratives
        exclusions.addAll(Arrays.asList("mine","yours","his","hers","its","ours","theirs"));//possessives
        exclusions.addAll(Arrays.asList("i","you","he","she","it","we","they"));//personals
        exclusions.addAll(Arrays.asList("where", "who", "when", "what", "why", "whose", "which", "how"));//questions
        exclusions.addAll(Arrays.asList("there"));
        return exclusions;
    }

    
}
