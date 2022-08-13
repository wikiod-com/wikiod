---
title: "Text Classification"
slug: "text-classification"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Text classification with LibLinear
 - Create training instances from .arff file


    private static Instances getDataFromFile(String path) throws Exception{

        DataSource source = new DataSource(path);
        Instances data = source.getDataSet();
        
        if (data.classIndex() == -1){
            data.setClassIndex(data.numAttributes()-1);
            //last attribute as class index
        }
        
        return data;    
    }


----------

    Instances trainingData = getDataFromFile(pathToArffFile);

- Use **StringToWordVector** to transform your string attributes to number representation:

    *Important features of this filter:
       
    1. tf-idf representation
    2. stemming
    3. lowercase wrods
    4. stopwords
    5. n-gram representation*


        StringToWordVector() filter = new StringToWordVector();    
        filter.setWordsToKeep(1000000);
        if(useIdf){
            filter.setIDFTransform(true);
        }
        filter.setTFTransform(true);
        filter.setLowerCaseTokens(true);
        filter.setOutputWordCounts(true);
        filter.setMinTermFreq(minTermFreq);
        filter.setNormalizeDocLength(new SelectedTag(StringToWordVector.FILTER_NORMALIZE_ALL,StringToWordVector.TAGS_FILTER));
        NGramTokenizer t = new NGramTokenizer();
        t.setNGramMaxSize(maxGrams);
        t.setNGramMinSize(minGrams);    
        filter.setTokenizer(t);     
        WordsFromFile stopwords = new WordsFromFile();
        stopwords.setStopwords(new File("data/stopwords/stopwords.txt"));
        filter.setStopwordsHandler(stopwords);
        if (useStemmer){
            Stemmer s = new /*Iterated*/LovinsStemmer();
            filter.setStemmer(s);
        }
        filter.setInputFormat(trainingData);

 - Apply the filter to trainingData: `trainingData = Filter.useFilter(trainingData, filter); `

 - Create the LibLinear Classifier

     1. SVMType 0 below corresponds to the L2-regularized logistic regression
     2. Set `setProbabilityEstimates(true)` to print the output probalities

            
            Classifier cls = null;
            LibLINEAR liblinear = new LibLINEAR();
            liblinear.setSVMType(new SelectedTag(0, LibLINEAR.TAGS_SVMTYPE));
            liblinear.setProbabilityEstimates(true);
            // liblinear.setBias(1); // default value
            cls = liblinear;
            cls.buildClassifier(trainingData);

 - Save model


        System.out.println("Saving the model...");
        ObjectOutputStream oos;
        oos = new ObjectOutputStream(new FileOutputStream(path+"mymodel.model"));
        oos.writeObject(cls);
        oos.flush();
        oos.close();

 - Create testing instances from `.arff` file

   

     Instances trainingData = getDataFromFile(pathToArffFile);

 - Load classifier


    Classifier myCls = (Classifier) weka.core.SerializationHelper.read(path+"mymodel.model");

 - **Use the same StringToWordVector filter as above or create a new one for testingData, but remember to use the trainingData for this command:` filter.setInputFormat(trainingData);`** *This will make training and testing instances compatible.
Alternatively you could use `InputMappedClassifier`*

 - Apply the filter to testingData: `testingData = Filter.useFilter(testingData, filter); `

 - Classify!

   1.Get the class value for every instance in the testing set

    
     for (int j = 0; j < testingData.numInstances(); j++) {
        double res = myCls.classifyInstance(testingData.get(j));
     }
*`res` is a double value that corresponds to the nominal class that is defined in `.arff` file. To get the nominal class use : `testintData.classAttribute().value((int)res)`*

    

----------
   2.Get the probability distribution for every instance

     for (int j = 0; j < testingData.numInstances(); j++) {
        double[] dist = first.distributionForInstance(testInstances.get(j));
     }
*`dist` is a double array that contains the probabilities for every class defined in `.arff` file*

*Note. Classifier should support probability distributions and enable them with: `myClassifier.setProbabilityEstimates(true);`*


     


