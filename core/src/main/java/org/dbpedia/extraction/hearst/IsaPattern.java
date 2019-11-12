package org.dbpedia.extraction.hearst;

import edu.stanford.nlp.ling.TaggedWord;
import java.util.ArrayList;
import java.util.List;


public class IsaPattern {
    private ArrayList<NounPhrase> instance;
    private ArrayList<NounPhrase> clazz;

    public IsaPattern(ArrayList<NounPhrase> instance, ArrayList<NounPhrase> clazz) {
        this.instance = (ArrayList<NounPhrase>)instance.clone();
        this.clazz = (ArrayList<NounPhrase>)clazz.clone();
    }

    public ArrayList<NounPhrase> getInstance() {
        return instance;
    }

    public ArrayList<NounPhrase> getClazz() {
        return clazz;
    }

    @Override
    public String toString() {
        return nounPhraseListUnderscoreToString(instance) + " --isa--> " + nounPhraseListUnderscoreToString(clazz);
    }
    
    
    private static String nounPhraseListToString(ArrayList<NounPhrase> nps) {
        if (nps.size() == 0) {
            return "{}";
        }
        StringBuilder result = new StringBuilder();
        result.append("{");
        for (NounPhrase np : nps) {
            result.append(np.toString()).append("|");
        }
        result.setLength(result.length() - 1);
        result.append("}");
        return result.toString();
    }
    
    private static String nounPhraseListUnderscoreToString(ArrayList<NounPhrase> nps) {
        if (nps.size() == 0) {
            return "{}";
        }
        StringBuilder result = new StringBuilder();
        result.append("{");
        for (NounPhrase np : nps) {
            for (TaggedWord tw : np.getPreModifier()) {
                result.append(tw.word()).append(" ");
            }
            result.append("_");
            result.append(np.getNPCore().word());
            result.append("_");
            for (TaggedWord tw : np.getPostModifier()) {
                result.append(tw.word()).append(" ");
            }
            //if (result.length() > 0) {
            //    result.setLength(result.length() - 1);
            //}
        
            result.append("|");
        }
        result.setLength(result.length() - 1);
        result.append("}");
        return result.toString();
    }
}
