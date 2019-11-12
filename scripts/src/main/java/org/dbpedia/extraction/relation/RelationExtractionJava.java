package org.dbpedia.extraction.relation;

import org.dbpedia.extraction.config.provenance.DBpediaDatasets;
import org.dbpedia.extraction.transform.Quad;
import org.dbpedia.extraction.util.Language;
import org.semanticweb.yars.nx.Node;
import org.semanticweb.yars.nx.Resource;
import org.semanticweb.yars.nx.parser.NxParser;
import weka.classifiers.Classifier;
import weka.classifiers.evaluation.Evaluation;
import weka.classifiers.trees.RandomForest;
import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.*;

public class RelationExtractionJava
{
    public static String baseDir = "";
    public static String suffix = "";



    public static List<Quad> run(Language language) throws Exception {
        Map<String, Integer> abstractEnds = getAbstractEnds();
        Map<String, Map<String, List<String>>> relations = getRelations();

        Map<String, RelationPage> training = getTrainingSet(abstractEnds, relations);

        updateType(training);
        computeSentenceFeatures(training, abstractEnds);
        computeBackLinksFeatures(training);

        Map<String, Classifier> models = trainModels(training);

        Map<String, Set<String>> predictionCandidates = predictionCandidates(models, relations);

        Map<String, RelationPage> predictionSet = getPredictionSet(abstractEnds, predictionCandidates);
        updateType(predictionSet);
        computeSentenceFeatures(predictionSet, abstractEnds);
        computeBackLinksFeatures(predictionSet);

        return makePrediction(predictionSet, models, language);
    }


    private static List<Quad> makePrediction(Map<String, RelationPage> predictionSet, Map<String, Classifier> models, Language language) throws Exception {

        Attribute f00 = new Attribute("F00");
        Attribute f01 = new Attribute("F01");
        Attribute f02 = new Attribute("F02");
        Attribute f03 = new Attribute("F03");
        Attribute f04 = new Attribute("F04");
        Attribute f05 = new Attribute("F05");
        Attribute f06 = new Attribute("F06");
        Attribute f07 = new Attribute("F07");
        Attribute f08 = new Attribute("F08");
        //Attribute f09 = new Attribute("F09");
        Attribute f10 = new Attribute("F10", Arrays.asList("0", "1"));//class
        ArrayList<Attribute> attributes = new ArrayList<Attribute>(
                Arrays.asList(f00, f01, f02, f03, f04, f05, f06, f07, f08, f10));



        List<Quad> quads = new ArrayList<>();
        for(Map.Entry<String, RelationPage> entry : predictionSet.entrySet()) {
            String subject = entry.getKey();
            RelationPage p = entry.getValue();
            List<RelationPropertyRelated> global = p.getPredicate2RelationRelated().get("global");

            for (Map.Entry<String, List<RelationPropertyRelated>> prop2RelationRelated : p.getPredicate2RelationRelated().entrySet()) {
                String property = prop2RelationRelated.getKey();
                if(property.equals("global"))
                    continue;
                Classifier cls = models.get(property);

                Instances instances = new Instances("Prediction", attributes, 10);
                instances.setClass(f10);

                for(int i = 0; i< prop2RelationRelated.getValue().size(); i++){
                    RelationPropertyRelated rpr = prop2RelationRelated.getValue().get(i);

                    Instance inst = new DenseInstance(attributes.size());

                    inst.setDataset(instances);

                    inst.setValue(f00, p.getNumberOfCandidates(property) == 0 ? 1.0f : 1.0f/(float)p.getNumberOfCandidates(property));

                    inst.setValue(f01, rpr.getCountInSentence());
                    inst.setValue(f02, rpr.getAbsolutePosition());
                    inst.setValue(f03, rpr.getPositionInSentence());

                    inst.setValue(f04, global.get(i).getCountInSentence());
                    inst.setValue(f05, global.get(i).getAbsolutePosition());
                    inst.setValue(f06, global.get(i).getPositionInSentence());

                    inst.setValue(f07, rpr.getLink().getSentencePosition());
                    inst.setValue(f08, rpr.getLink().isBackLink() ? 1.0f : 0.0f);

                    //inst.setValue(f10, rpr.isClazz() ? 1.0f : 0.0f);
                    double test = cls.classifyInstance(inst);
                    if(test == 1.0){
                        //System.out.println(subject + "  ->  " + property + "  ->  " + rpr.getLink().getLinkTarget());
                        quads.add(new Quad(language, DBpediaDatasets.RelationExtraction(), subject, property, rpr.getLink().getLinkTarget(), subject, null));
                    }

                }
            }
        }
        return quads;
    }



    private static Map<String, Set<String>> predictionCandidates(Map<String, Classifier> models, Map<String, Map<String, List<String>>> relations) throws FileNotFoundException {
        Map<String, String> propertyDomain = loadPropertyDomain();
        Map<String, Set<String>> type2Subject = loadType2Subjects();

        Map<String, Set<String>> subjects2predicates = new HashMap<>();
        for(Map.Entry<String, Classifier> entry : models.entrySet() ){
            String predicate = entry.getKey();
            for(String subject : type2Subject.getOrDefault(propertyDomain.getOrDefault(predicate, ""), new HashSet<>())){
                if(relations.getOrDefault(subject, new HashMap<>()).getOrDefault(predicate, new ArrayList<>()).size() > 0)
                    continue;
                Set<String> predicates = subjects2predicates.get(subject);
                if(predicates == null){
                    predicates = new HashSet<>();
                    subjects2predicates.put(subject, predicates);
                }
                predicates.add(predicate);
            }
        }
        return subjects2predicates;
    }


    private static Map<String, Classifier> trainModels(Map<String, RelationPage> training) throws Exception {
        Attribute f00 = new Attribute("F00");
        Attribute f01 = new Attribute("F01");
        Attribute f02 = new Attribute("F02");
        Attribute f03 = new Attribute("F03");
        Attribute f04 = new Attribute("F04");
        Attribute f05 = new Attribute("F05");
        Attribute f06 = new Attribute("F06");
        Attribute f07 = new Attribute("F07");
        Attribute f08 = new Attribute("F08");
        //Attribute f09 = new Attribute("F09");
        Attribute f10 = new Attribute("F10", Arrays.asList("0", "1"));//class
        ArrayList<Attribute> attributes = new ArrayList<Attribute>(
                Arrays.asList(f00, f01, f02, f03, f04, f05, f06, f07, f08, f10));


        Map<String, Instances> wekaTraining = new HashMap<>();

        for(Map.Entry<String, RelationPage> entry : training.entrySet()) {
            RelationPage p = entry.getValue();
            List<RelationPropertyRelated> global = p.getPredicate2RelationRelated().get("global");

            for (Map.Entry<String, List<RelationPropertyRelated>> prop2RelationRelated : p.getPredicate2RelationRelated().entrySet()) {
                String property = prop2RelationRelated.getKey();
                if(property.equals("global"))
                    continue;
                Instances instances = wekaTraining.get(property);
                if(instances == null){
                    instances = new Instances("Training", attributes, training.size());
                    instances.setClass(f10);
                    wekaTraining.put(property, instances);
                }

                for(int i = 0; i< prop2RelationRelated.getValue().size(); i++){
                    RelationPropertyRelated rpr = prop2RelationRelated.getValue().get(i);

                    Instance inst = new DenseInstance(attributes.size());
                    inst.setDataset(instances);



                    inst.setValue(f00, p.getNumberOfCandidates(property) == 0 ? 1.0f : 1.0f/(float)p.getNumberOfCandidates(property));

                    inst.setValue(f01, rpr.getCountInSentence());
                    inst.setValue(f02, rpr.getAbsolutePosition());
                    inst.setValue(f03, rpr.getPositionInSentence());

                    inst.setValue(f04, global.get(i).getCountInSentence());
                    inst.setValue(f05, global.get(i).getAbsolutePosition());
                    inst.setValue(f06, global.get(i).getPositionInSentence());

                    inst.setValue(f07, rpr.getLink().getSentencePosition());
                    inst.setValue(f08, rpr.getLink().isBackLink() ? 1.0f : 0.0f);

                    inst.setValue(f10, rpr.isClazz() ? 1.0f : 0.0f);

                    instances.add(inst);
                }

            }
        }


        Map<String, Classifier> models = new HashMap<>();

        for(Map.Entry<String, Instances> entry : wekaTraining.entrySet()){
            //System.out.println("Property: " + entry.getKey());
            if(entry.getValue().size() < 10){
                //System.out.println("too few training");
                continue;
            }

            Classifier cls = new RandomForest();
            Evaluation eval = new Evaluation(entry.getValue());
            Random rand = new Random(1);  // using seed = 1
            int folds = 10;
            eval.crossValidateModel(cls, entry.getValue(), folds, rand);

            if(eval.precision(1)>0.95){
                System.out.println("Property: " + entry.getKey() + "  " + eval.precision(1));

                cls.buildClassifier(entry.getValue());
                models.put(entry.getKey(), cls);
            }
        }

        return models;
    }



    private static Map<String, RelationPage> getPredictionSet(Map<String, Integer> abstractEnds, Map<String, Set<String>> subjects2predicates) throws FileNotFoundException {

        Map<String, RelationPage> predictionSet = new HashMap<>();

        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "nif-text-links" + suffix));

        String currentResource = "";
        int currentAbstractEnd = 0;

        Set<RelationLink> allLinks = new HashSet<>();
        Set<String> predicates = new HashSet<>();
        for (Node[] nx : nxp){
            if(nx[2] instanceof Resource == false || nx[1].getLabel().equals("http://www.w3.org/2005/11/its/rdf#taIdentRef") == false) {
                continue;
            }
            String subject = nx[0].getLabel();
            int pathQuerySep = subject.indexOf("?");
            String path = subject.substring(0, pathQuerySep);
            String query = subject.substring(pathQuerySep + 1);

            RelationLink l = getLink(nx[2].getLabel(), query);

            //assumption: file is clustered by subject -> this means when a subject changes from one line to the other, this previous subject will not appear again
            if(currentResource.equals(path) == false){
                if(allLinks.size() > 0 && predicates!= null && predicates.size() > 0){
                    RelationPage p = new RelationPage(allLinks);
                    for(String predicate : predicates){
                        p.add(predicate, "");
                    }
                    predictionSet.put(currentResource, p);
                }

                currentResource = path;
                currentAbstractEnd = abstractEnds.getOrDefault(currentResource, 0);
                allLinks = new HashSet<>();
                predicates = subjects2predicates.get(path);
            }
            if(l.getEnd() < currentAbstractEnd) {//only abstracts - comment this to use all links
                allLinks.add(l);
            }
        }
        return predictionSet;
    }



    private static Map<String, RelationPage> getTrainingSet(Map<String, Integer> abstractEnds, Map<String, Map<String, List<String>>> relations) throws FileNotFoundException {

        Map<String, RelationPage> trainingSet = new HashMap<>();


        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "nif-text-links" + suffix));

        String currentResource = "";
        int currentAbstractEnd = 0;

        Set<String> seenLinkTargets = new HashSet<>();
        Set<RelationLink> allLinks = new HashSet<>();

        for (Node[] nx : nxp){
            if(nx[2] instanceof Resource == false || nx[1].getLabel().equals("http://www.w3.org/2005/11/its/rdf#taIdentRef") == false) {
                continue;
            }
            String subject = nx[0].getLabel();
            int pathQuerySep = subject.indexOf("?");
            String path = subject.substring(0, pathQuerySep);
            String query = subject.substring(pathQuerySep + 1);

            RelationLink l = getLink(nx[2].getLabel(), query);

            //assumption: file is clustered by subject -> this means when a subject changes from one line to the other, this previous subject will not appear again
            if(currentResource.equals(path) == false){
                RelationPage p = processPage(seenLinkTargets, allLinks, relations.getOrDefault(currentResource, new HashMap<>()));
                if(p.containsLearningMaterial())
                    trainingSet.put(currentResource, p);

                currentResource = path;
                currentAbstractEnd = abstractEnds.getOrDefault(currentResource, 0);
                seenLinkTargets = new HashSet<>();
                allLinks = new HashSet<>();
            }
            if(l.getEnd() < currentAbstractEnd) {//only abstracts - comment this to use all links
                seenLinkTargets.add(l.getLinkTarget());
                allLinks.add(l);
            }
        }
        return trainingSet;
    }


    private static void updateType(Map<String, RelationPage> training) throws FileNotFoundException {
        Map<String, String> ranges = loadPropertyRange();
        Map<String, Set<String>> typeMap = loadTypes();
        for(Map.Entry<String, RelationPage> entry : training.entrySet()){
            for(Map.Entry<String, List<RelationPropertyRelated>> prop2RelationRelated : entry.getValue().getPredicate2RelationRelated().entrySet()){
                String property = prop2RelationRelated.getKey();
                String range = ranges.getOrDefault(property, "");

                for(RelationPropertyRelated r : prop2RelationRelated.getValue()){
                    Set<String> types = typeMap.getOrDefault(r.getLink().getLinkTarget(), new HashSet<>());
                    r.setEqualTypes(types.contains(range));
                }
            }
        }
    }

    private static RelationPage processPage(Set<String> seenLinkTargets, Set<RelationLink> allLinks, Map<String, List<String>> infoboxPredicate2Objects){

        RelationPage p = new RelationPage(allLinks);
        if(seenLinkTargets.size() == 0)
            return p;
        for(Map.Entry<String, List<String>> predicate2Objects : infoboxPredicate2Objects.entrySet()){
            String predicate = predicate2Objects.getKey();
            for(String object : predicate2Objects.getValue()) {
                if (seenLinkTargets.contains(object)) {//this is local closed world assumption
                    p.add(predicate, object);
                }
            }
        }
        return p;
    }





    private static void computeSentenceFeatures(Map<String, RelationPage> training, Map<String, Integer> abstractEnds) throws FileNotFoundException {


        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "nif-context" + suffix));

        for (Node[] nx : nxp){
            if(nx[1].getLabel().equals("http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#isString") == false) {
                continue;
            }
            String subject = nx[0].getLabel();
            int pathQuerySep = subject.indexOf("?");
            String subjectPath = subject.substring(0, pathQuerySep);
            String fullWikiText = nx[2].getLabel();


            RelationPage p = training.get(subjectPath);
            if(p == null)
                continue;
            p.addGlobal();
            Set<String> predicates = p.getPredicate2RelationRelated().keySet();


            int totalNumberOfCanadiates = p.getNumberOfEntities(); //F00

            String abstractText = fullWikiText.substring(0, abstractEnds.getOrDefault(subjectPath, 0));
            List<Integer> sentenceSplitIndices = RelationUtil.getAllIndices(abstractText, ". ");


            //int[] candidatesInSentence = new int[sentenceSplitIndices.size() + 1];
            //int counterLinksBefore = 0;

            Map<String, Integer> counterLinksBeforeMap = new HashMap<>();
            for(String predicate : predicates)
                counterLinksBeforeMap.put(predicate, 0);

            Map<String, int[]> candidatesInSentenceMap = new HashMap<>();
            for(String predicate : predicates)
                candidatesInSentenceMap.put(predicate, new int[sentenceSplitIndices.size() + 1]);

            for(int i = 0; i < p.getLinksOrderedByStart().size() ; i++){
                RelationLink l = p.getLinksOrderedByStart().get(i);

                int binSearch = Collections.binarySearch(sentenceSplitIndices, l.getEnd());
                int sentenceIndex = binSearch < 0 ? ~binSearch : binSearch + 1;

                l.setSentencePosition(sentenceIndex == 0 ? 1.0f : 1.0f / (2.0f * (float)sentenceIndex));

                for(String predicate : predicates) {
                    RelationPropertyRelated rpr = p.getPredicate2RelationRelated().get(predicate).get(i);
                    if(rpr.isEqualTypes()){
                        Integer counterLinksBefore = counterLinksBeforeMap.get(predicate);
                        int[] candidatesInSentence = candidatesInSentenceMap.get(predicate);

                        rpr.setAbsolutePosition(counterLinksBefore == 0 ? 1.0f : 1.0f / (2.0f * (float)counterLinksBefore));
                        rpr.setPositionInSentence(candidatesInSentence[sentenceIndex] == 0 ? 1.0f : 1.0f / (2.0f * (float)candidatesInSentence[sentenceIndex]));

                        counterLinksBeforeMap.put(predicate, ++counterLinksBefore);
                        candidatesInSentence[sentenceIndex]++;
                    }
                }
            }

            for(int i = 0; i < p.getLinksOrderedByStart().size() ; i++){
                RelationLink l = p.getLinksOrderedByStart().get(i);

                int binSearch = Collections.binarySearch(sentenceSplitIndices, l.getEnd());
                int sentenceIndex = binSearch < 0 ? ~binSearch : binSearch + 1;

                for(String predicate : predicates) {
                    RelationPropertyRelated rpr = p.getPredicate2RelationRelated().get(predicate).get(i);
                    if(rpr.isEqualTypes()){
                        int[] candidatesInSentence = candidatesInSentenceMap.get(predicate);

                        rpr.setCountInSentence(1.0f/(float)candidatesInSentence[sentenceIndex]);
                    }
                }
            }
        }

    }

    private static void computeBackLinksFeatures(Map<String, RelationPage> training) throws FileNotFoundException {
        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "page-links" + suffix));

        Map<String, Set<String>> wikiPageLinks = new HashMap<>();
        for (Node[] nx : nxp) {
            String subject = nx[0].getLabel();
            String pageLink = nx[2].getLabel();

            Set<String> links = wikiPageLinks.get(subject);
            if(links == null){
                links = new HashSet<>();
                wikiPageLinks.put(subject, links);
            }
            links.add(pageLink);
        }

        for(Map.Entry<String, RelationPage> entry : training.entrySet()){
            String subject = entry.getKey();
            for(RelationLink l : entry.getValue().getLinksOrderedByStart()){
                Set<String> links = wikiPageLinks.getOrDefault(l.getLinkTarget(), new HashSet<>());
                if(links.contains(subject)){
                    l.setBackLink(true);
                }
            }
        }
    }




    private static Map<String, Integer> getAbstractEnds() throws FileNotFoundException {
        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "nif-page-structure" + suffix));
        Map<String, Integer> abstractEnds = new HashMap<>();
        for (Node[] nx : nxp) {
            if (nx[1].getLabel().equals("http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#firstSection") == false)
                continue;

            String object = nx[2].getLabel();
            int pathQuerySep = object.indexOf("?");
            Map<String, String> queryPairs = RelationUtil.splitQuery(object.substring(pathQuerySep + 1));

            String charPositions = queryPairs.get("char");
            if (charPositions == null)
                continue;
            String[] startEnd = charPositions.split(",");
            if (startEnd.length != 2)
                continue;

            if (startEnd[0].equals("0") == false)
                continue;

            try {
                abstractEnds.put(object.substring(0, pathQuerySep), Integer.parseInt(startEnd[1]));
            } catch (NumberFormatException e) {
                continue;
            }
        }
        return abstractEnds;
    }


    private static Map<String, Map<String, List<String>>> getRelations() throws FileNotFoundException {
        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "infobox-properties" + suffix));
        Map<String, Map<String, List<String>>> relations = new HashMap<>();
        for (Node[] nx : nxp){
            if(nx[2] instanceof Resource == false) {
                continue;
            }
            RelationUtil.addtoMap(relations, nx[0].getLabel(), nx[1].getLabel(), nx[2].getLabel());
        }
        return relations;
    }

    private static Map<String, String> loadPropertyRange() throws FileNotFoundException {
        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "infobox-property-restrictions" + suffix));
        Map<String, String> ranges = new HashMap<>();
        for (Node[] nx : nxp) {
            if (nx[1].getLabel().equals("http://www.w3.org/2000/01/rdf-schema#range") == false) {
                continue;
            }
            ranges.put(nx[0].getLabel(), nx[2].getLabel());
        }
        return ranges;
    }

    private static Map<String, String> loadPropertyDomain() throws FileNotFoundException {
        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "infobox-property-restrictions" + suffix));
        Map<String, String> domain = new HashMap<>();
        for (Node[] nx : nxp) {
            if (nx[1].getLabel().equals("http://www.w3.org/2000/01/rdf-schema#domain") == false) {
                continue;
            }
            domain.put(nx[0].getLabel(), nx[2].getLabel());
        }
        return domain;
    }

    private static Map<String, Set<String>> loadTypes() throws FileNotFoundException {
        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "template-type" + suffix));
        Map<String, Set<String>> typeMap = new HashMap<>();
        for (Node[] nx : nxp) {
            if (nx[1].getLabel().equals("http://www.w3.org/1999/02/22-rdf-syntax-ns#type") == false) {
                continue;
            }
            Set<String> types = typeMap.get(nx[0].getLabel());
            if(types == null){
                types = new HashSet<>();
                typeMap.put(nx[0].getLabel(), types);
            }
            types.add(nx[2].getLabel());
        }

        nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "sd-types-light" + suffix));
        for (Node[] nx : nxp) {
            if (nx[1].getLabel().equals("http://www.w3.org/1999/02/22-rdf-syntax-ns#type") == false) {
                continue;
            }
            Set<String> types = typeMap.get(nx[0].getLabel());
            if(types == null){
                types = new HashSet<>();
                typeMap.put(nx[0].getLabel(), types);
            }
            types.add(nx[2].getLabel());
        }
        return typeMap;
    }

    private static Map<String, Set<String>> loadType2Subjects() throws FileNotFoundException {
        NxParser nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "template-type" + suffix));
        Map<String, Set<String>> typeMap = new HashMap<>();
        for (Node[] nx : nxp) {
            if (nx[1].getLabel().equals("http://www.w3.org/1999/02/22-rdf-syntax-ns#type") == false) {
                continue;
            }
            Set<String> types = typeMap.get(nx[2].getLabel());
            if(types == null){
                types = new HashSet<>();
                typeMap.put(nx[2].getLabel(), types);
            }
            types.add(nx[0].getLabel());
        }

        nxp = new NxParser();
        nxp.parse(new FileInputStream(baseDir + "sd-types-light" + suffix));
        for (Node[] nx : nxp) {
            if (nx[1].getLabel().equals("http://www.w3.org/1999/02/22-rdf-syntax-ns#type") == false) {
                continue;
            }
            Set<String> types = typeMap.get(nx[2].getLabel());
            if(types == null){
                types = new HashSet<>();
                typeMap.put(nx[2].getLabel(), types);
            }
            types.add(nx[0].getLabel());
        }
        return typeMap;
    }

    private static RelationLink getLink(String linkTarget, String query){
        String[] startEnd = RelationUtil.splitQuery(query).getOrDefault("char", "0,0").split(",");
        int start = 0;
        int end = 0;
        try{
            start = Integer.parseInt(startEnd[0]);
        }catch (NumberFormatException e){}
        try{
            end = Integer.parseInt(startEnd[1]);
        }catch (NumberFormatException e){}
        return new RelationLink(linkTarget,start, end);
    }
}

class RelationUtil{
    public static <T> void addtoMap(Map<String, Map<String, List<T>>> map, String first, String second,  T third){
        Map<String, List<T>> second_third_map = map.get(first);
        if(second_third_map == null){
            second_third_map = new HashMap<>();
            map.put(first, second_third_map);
        }
        List<T> list = second_third_map.get(second);
        if(list == null){
            list = new LinkedList<>();
            second_third_map.put(second, list);
        }
        list.add(third);
    }

    public static Map<String, String> splitQuery(String queryPart) {
        Map<String, String> query_pairs = new HashMap<>();
        for (String pair : queryPart.split("&")) {
            int idx = pair.indexOf("=");
            String key = idx > 0 ? pair.substring(0, idx) : pair;
            String value = idx > 0 && pair.length() > idx + 1 ? pair.substring(idx + 1) : null;
            query_pairs.put(key, value);
        }
        return query_pairs;
    }

    public static int getIndexInArray(String[] array, String searchValue){
        for(int i = 0; i < array.length; i++){
            if(array[i].contains(searchValue))
                return i;
        }
        return 0;
    }

    public static List<Integer> getAllIndices(String wholeString, String searchValue){
        List<Integer> indices = new ArrayList<>();
        for (int index = wholeString.indexOf(searchValue); index >= 0; index = wholeString.indexOf(searchValue, index + 1))
        {
            indices.add(index);
        }
        return indices;
    }
}

class RelationPage {

    private List<RelationLink> links;
    private Map<String, List<RelationPropertyRelated>> predicate2RelationRelated;

    public RelationPage(Set<RelationLink> _links){
        this.links = new ArrayList<>(_links);
        Collections.sort(this.links, Comparator.comparingInt(RelationLink::getStart));
        this.predicate2RelationRelated = new HashMap<>();
    }

    public void add(String predicate, String object){
        List<RelationPropertyRelated> objects = this.predicate2RelationRelated.get(predicate);
        if(objects == null) {
            objects = new ArrayList<>();
            this.predicate2RelationRelated.put(predicate, objects);
            for (RelationLink l : links) {
                if (l.getLinkTarget().equals(object))
                    objects.add(new RelationPropertyRelated(true, l));
                else
                    objects.add(new RelationPropertyRelated(false, l));
            }
        }else{
            for(RelationPropertyRelated r : objects){
                if(r.getLink().getLinkTarget().equals(object))
                    r.setClazz(true);
            }
        }
    }

    public void addGlobal(){
        List<RelationPropertyRelated> objects = new ArrayList<>();
        for (RelationLink l : links) {
            RelationPropertyRelated rpr = new RelationPropertyRelated(false, l);
            rpr.setEqualTypes(true);
            objects.add(rpr);
        }
        this.predicate2RelationRelated.put("global", objects);
    }

    public boolean containsLearningMaterial(){
        return this.predicate2RelationRelated.size() > 0;
    }

    public List<RelationLink> getLinksOrderedByStart(){
        return this.links;
    }

    public Map<String, List<RelationPropertyRelated>> getPredicate2RelationRelated(){
        return this.predicate2RelationRelated;
    }


    public int getNumberOfEntities(){
        return this.links.size();
    }

    public int getNumberOfCandidates(String prop){
        int candidates = 0;
        for(RelationPropertyRelated rpr :this.predicate2RelationRelated.getOrDefault(prop, new ArrayList<>()) ){
            if(rpr.isEqualTypes())
                candidates++;
        }
        return candidates;
    }

}
class RelationLink{
    private String linkTarget;
    private int start;
    private int end;
    //features:
    private float sentencePosition;
    private boolean backLink;

    public RelationLink(String _linkTarget, int _start, int _end){
        this.linkTarget = _linkTarget;
        this.start = _start;
        this.end = _end;
        this.sentencePosition = 0.0f;
        this.backLink = false;
    }


    public String getLinkTarget(){
        return linkTarget;
    }

    public int getStart() {
        return start;
    }

    public int getEnd() {
        return end;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RelationLink that = (RelationLink) o;
        return start == that.start &&
                end == that.end &&
                Objects.equals(linkTarget, that.linkTarget);
    }

    @Override
    public int hashCode() {
        return Objects.hash(linkTarget, start, end);
    }

    public float getSentencePosition() {
        return sentencePosition;
    }

    public void setSentencePosition(float sentencePosition) {
        this.sentencePosition = sentencePosition;
    }

    public boolean isBackLink() {
        return backLink;
    }

    public void setBackLink(boolean backLink) {
        this.backLink = backLink;
    }
}

class RelationPropertyRelated{
    private boolean clazz;
    private boolean equalTypes;
    private RelationLink link;
    //features:
    private float absolutePosition;
    private float positionInSentence;
    private float countInSentence;


    public RelationPropertyRelated(boolean _clazz, RelationLink _link){
        this.clazz = _clazz;
        this.link = _link;
    }

    public boolean isClazz() {
        return clazz;
    }

    public void setClazz(boolean clazz) {
        this.clazz = clazz;
    }

    public RelationLink getLink(){
        return this.link;
    }

    public boolean isEqualTypes() {
        return equalTypes;
    }

    public void setEqualTypes(boolean equalTypes) {
        this.equalTypes = equalTypes;
    }


    public double getAbsolutePosition() {
        return absolutePosition;
    }

    public void setAbsolutePosition(float absolutePosition) {
        this.absolutePosition = absolutePosition;
    }

    public float getPositionInSentence() {
        return positionInSentence;
    }

    public void setPositionInSentence(float positionInSentence) {
        this.positionInSentence = positionInSentence;
    }

    public float getCountInSentence() {
        return countInSentence;
    }

    public void setCountInSentence(float countInSentence) {
        this.countInSentence = countInSentence;
    }
}
