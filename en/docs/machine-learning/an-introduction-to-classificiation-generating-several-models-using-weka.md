---
title: "An introduction to Classificiation Generating several models using Weka"
slug: "an-introduction-to-classificiation-generating-several-models-using-weka"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

This tutorial will show you how to use Weka in JAVA code, load data file,  train classifiers and explains some of important concepts behind machine learning.

Weka is a toolkit for machine learning. It includes a library of machine learning and visualisation techniques and features a user friendly GUI.

This tutorial includes examples written in JAVA and includes visuals generated with the GUI. I suggest using the GUI to examine data and JAVA code for structured experiments.

## Getting started: Loading a dataset from file
The [Iris flower data set][1] is a widely used data set for demonstration purposes. We will load it, inspect it and slightly modify it for later use.
    
    import java.io.File;
    import java.net.URL;
    import weka.core.Instances;
    import weka.core.converters.ArffSaver;
    import weka.core.converters.CSVLoader;
    import weka.filters.Filter;
    import weka.filters.unsupervised.attribute.RenameAttribute;
    import weka.classifiers.evaluation.Evaluation;
    import weka.classifiers.rules.ZeroR;
    import weka.classifiers.bayes.NaiveBayes;
    import weka.classifiers.lazy.IBk;
    import weka.classifiers.trees.J48;
    import weka.classifiers.meta.AdaBoostM1;
    
    public class IrisExperiments {
        public static void main(String args[]) throws Exception
         {
            //First we open stream to a data set as provided on http://archive.ics.uci.edu
             CSVLoader loader = new CSVLoader();
             loader.setSource(new URL("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data").openStream());
             Instances data = loader.getDataSet();
             
             //This file has 149 examples with 5 attributes
             //In order:
             //  sepal length in cm
             //  sepal width in cm
             //  petal length in cm 
             //  petal width in cm
             //  class ( Iris Setosa , Iris Versicolour, Iris Virginica)
    
             //Let's briefly inspect the data
             System.out.println("This file has " + data.numInstances()+" examples.");
             System.out.println("The first example looks like this: ");
             for(int i = 0; i < data.instance(0).numAttributes();i++ ){
                 System.out.println(data.instance(0).attribute(i));
             } 
             
             // NOTE that the last attribute is Nominal
             // It is convention to have a nominal variable at the last index as target variable
             
             // Let's tidy up the data a little bit
             // Nothing too serious just to show how we can manipulate the data with filters
             RenameAttribute renamer = new RenameAttribute();
             renamer.setOptions(weka.core.Utils.splitOptions("-R last -replace Iris-type"));
             renamer.setInputFormat(data);                          
             data = Filter.useFilter(data, renamer); 
             
             System.out.println("We changed the name of the target class.");
             System.out.println("And now it looks like this:");
             System.out.println(data.instance(0).attribute(4));
             
             //Now we do this for all the attributes
             renamer.setOptions(weka.core.Utils.splitOptions("-R 1 -replace sepal-length"));
             renamer.setInputFormat(data);                          
             data = Filter.useFilter(data, renamer); 
             
             renamer.setOptions(weka.core.Utils.splitOptions("-R 2 -replace sepal-width"));
             renamer.setInputFormat(data);                          
             data = Filter.useFilter(data, renamer); 
             
             renamer.setOptions(weka.core.Utils.splitOptions("-R 3 -replace petal-length"));
             renamer.setInputFormat(data);                          
             data = Filter.useFilter(data, renamer); 
             
             renamer.setOptions(weka.core.Utils.splitOptions("-R 4 -replace petal-width"));
             renamer.setInputFormat(data);                          
             data = Filter.useFilter(data, renamer); 
             
             //Lastly we save our newly created file to disk
             ArffSaver saver = new ArffSaver();
             saver.setInstances(data);
             saver.setFile(new File("IrisSet.arff"));
             saver.writeBatch();
      }
    }


  [1]: https://en.wikipedia.org/wiki/Iris_flower_data_set

## Train the first classifier: Setting a baseline with ZeroR
ZeroR is a simple classifier. It doesn't operate per instance instead it operates on general distribution of the classes. It selects the class with the largest a priori probability. It is not a good classifier in the sense that it doesn't use any information in the candidate, but it is often used as a baseline. **Note: Other baselines can be used aswel, such as: Industry standard classifiers or handcrafted rules**
     
     // First we tell our data that it's class is hidden in the last attribute
     data.setClassIndex(data.numAttributes() -1);
     // Then we split the data in to two sets
     // randomize first because we don't want unequal distributions
     data.randomize(new java.util.Random(0));
     Instances testset = new Instances(data, 0, 50);
     Instances trainset = new Instances(data, 50, 99);
     
     // Now we build a classifier
     // Train it with the trainset
     ZeroR classifier1 = new ZeroR();
     classifier1.buildClassifier(trainset);
     // Next we test it against the testset
     Evaluation Test = new Evaluation(trainset);
     Test.evaluateModel(classifier1, testset);
     System.out.println(Test.toSummaryString());

The largest class in the set gives you a 34% correct rate. (50 out of 149)

[![The actual distribution of the classes in the set][1]][1]

**Note: The ZeroR performs around 30%. This is because we splitted randomly into a train and test set. The largest set in the train set, will thusly be the smallest in the test set. Crafting a good test/train set can be worth your while**



  [1]: https://i.stack.imgur.com/y1UJI.png

## Getting a feel for the data. Training Naive Bayes and kNN
In order to build a good classifier we will often need to get an idea of how the data is structered in feature space. Weka offers a visualisation module that can help.

[![plots of 2 dimensions, classes are colored in][1]][1]

Some dimensions already seperate the classes quite well. Petal-width orders the concept quite neatly, when compared to petal-width for instance.

Training simple classifiers can reveal quite some about the structure of the data too. I usually like to use Nearest Neighbor and Naive Bayes for that purpose. Naive Bayes assumes independence, it performing well is an indication that dimensions on itself hold information. k-Nearest-Neighbor works by assigning the class of the k nearest (known) instances in feature space. It is often used to examine local geographical dependence, we will use it to examine whether our concept is defined locally in feature space. 
     
     //Now we build a Naive Bayes classifier
     NaiveBayes classifier2 = new NaiveBayes();
     classifier2.buildClassifier(trainset);
     // Next we test it against the testset
     Test = new Evaluation(trainset);
     Test.evaluateModel(classifier2, testset);
     System.out.println(Test.toSummaryString());
     
    //Now we build a kNN classifier
    IBk classifier3 = new IBk();
    // We tell the classifier to use the first nearest neighbor as example 
    classifier3.setOptions(weka.core.Utils.splitOptions("-K 1"));
    classifier3.buildClassifier(trainset);
    // Next we test it against the testset
    Test = new Evaluation(trainset);
    Test.evaluateModel(classifier3, testset);
    System.out.println(Test.toSummaryString());

Naive Bayes performs much better than our freshly established baseline, indcating that independent features hold information (remember petal-width?). 

1NN performs well too (in fact a little better in this case), indicating that some of our information is local. The better performance could indicate that some second order effects also hold information *(If x and y than class z)*. 

  [1]: https://i.stack.imgur.com/FNRuu.png

## Putting it together: Training a tree
Trees can build models that work on independent features and on second order effects. So they might be good candidates for this domain. Trees are rules that are chaind together, a rule splits instances that arrive at a rule in subgroups, that pass to the rules under the rule. 

Tree Learners generate rules, chain them together and stop building trees when they feel the rules get too specific, to avoid overfitting. *Overfitting means constructing a model that is too complex for the concept we are looking for. Overfitted models perform well on the train data, but poorly on new data*

We use J48, a JAVA implementation of C4.5 a popular algorithm.
    
    //We train a tree using J48
    //J48 is a JAVA implementation of the C4.5 algorithm
    J48 classifier4 = new J48();
    //We set it's confidence level to 0.1
    //The confidence level tell J48 how specific a rule can be before it gets pruned
    classifier4.setOptions(weka.core.Utils.splitOptions("-C 0.1"));
    classifier4.buildClassifier(trainset);
    // Next we test it against the testset
    Test = new Evaluation(trainset);
    Test.evaluateModel(classifier4, testset);
    System.out.println(Test.toSummaryString());
    
    System.out.print(classifier4.toString());
    
    //We set it's confidence level to 0.5
    //Allowing the tree to maintain more complex rules
    classifier4.setOptions(weka.core.Utils.splitOptions("-C 0.5"));
    classifier4.buildClassifier(trainset);
    // Next we test it against the testset
    Test = new Evaluation(trainset);
    Test.evaluateModel(classifier4, testset);
    System.out.println(Test.toSummaryString());
         
    System.out.print(classifier4.toString());

The tree learner trained with the highest confidence generates the most specific rules, and has the best performance on the test set, appearently the specificness is warranted.

[![enter image description here][1]][1]
[![enter image description here][2]][2]

**Note: Both learners start with a rule on petal-width. Remember how we noticed this dimension in the visualization ?**

  [1]: https://i.stack.imgur.com/v1wQY.png
  [2]: https://i.stack.imgur.com/GyPpD.png

