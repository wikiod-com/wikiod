---
title: "Mistakes easily made when using KnowledgeFlow"
slug: "mistakes-easily-made-when-using-knowledgeflow"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Weka KnowledgeFlow(KF) is a great interface to use. However, Weka manual does not cover every little details of using KF. Here would be a place for collecting those little tricks or details I learnt from those errors I did or will make as time goes. Many thanks to people at Wekalist (especially Mark Hall, Eibe Frank) for building a wonderful learning environment for Weka!

## **TrainingSetMaker and TestSetMaker**

1. a `ClassAssigner` must be linked between `ArffLoader` and `TrainingSetMaker` or `TestSetMaker`.
----

## **ArffSaver**

2. In order to save dataset into arff file successfully, it is safer to set `relationNameForFilename` to False inside configuration of `ArffSaver`.
----

## **How to use TimeSeriesForecasting in KnowledgeFlow?**
1. Open knowledgeFlow, load dataset with ArffLoader
2. go to setting, check time series forecasting perspective, right-click ArffLoader to send to all perspective 
3. go to time series forecasting perspective to set up a model 
4. run the model and copy the model to clipboard 
5. ctrl + v, and click to paste model to Data mining process canvas 
6. save prediction along with original data with ArffSaver
----


## How to open KnowledgeFlow file directly from terminal
1. add the following function into `.bash_profile`, save and exit

function wekaflstart() {     
    export R_HOME=/Library/Frameworks/R.framework/Resources     
    java -Xss10M -Xmx4096M -cp :weka.jar weka.gui.knowledgeflow.KnowledgeFlow "$1"    
}

2. inside a directory with a `weka.jar` file, open its terminal, run `wekastart "path to a knowledgeflow file" `

