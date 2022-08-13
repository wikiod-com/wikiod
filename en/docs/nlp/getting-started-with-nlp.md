---
title: "Getting started with nlp"
slug: "getting-started-with-nlp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Stanford CoreNLP
[Stanford CoreNLP](http://stanfordnlp.github.io/CoreNLP/) is a popular Natural Language Processing toolkit supporting many core NLP tasks. 

To download and install the program, either download a release package and include the necessary `*.jar` files in your classpath, or add the dependency off of Maven central. See [the download page](http://stanfordnlp.github.io/CoreNLP/download.html) for more detail. For example:

    curl http://nlp.stanford.edu/software/stanford-corenlp-full-2015-12-09.zip -o corenlp.zip
    unzip corenlp.zip
    cd corenlp
    export CLASSPATH="$CLASSPATH:`pwd`/*

There are three supported ways to run the CoreNLP tools: (1) using the [base fully customizable API](http://stanfordnlp.github.io/CoreNLP/api.html), (2) using the [Simple CoreNLP](http://stanfordnlp.github.io/CoreNLP/simple.html) API, or (3) using the [CoreNLP server](http://stanfordnlp.github.io/CoreNLP/corenlp-server.html). A simple usage example for each is given below. As a motivating use case, these examples will be for predicting the syntactic parse of a sentence.

 1. **CoreNLP API**
    ```java
    public class CoreNLPDemo {
      public static void main(String[] args) {

        // 1. Set up a CoreNLP pipeline. This should be done once per type of annotation,
        //    as it's fairly slow to initialize.
        // creates a StanfordCoreNLP object, with POS tagging, lemmatization, NER, parsing, and coreference resolution 
        Properties props = new Properties();
        props.setProperty("annotators", "tokenize, ssplit, parse");
        StanfordCoreNLP pipeline = new StanfordCoreNLP(props);

        // 2. Run the pipeline on some text.
        // read some text in the text variable
        String text = "the quick brown fox jumped over the lazy dog"; // Add your text here!
        // create an empty Annotation just with the given text
        Annotation document = new Annotation(text);
        // run all Annotators on this text
        pipeline.annotate(document);

        // 3. Read off the result
        // Get the list of sentences in the document
        List<CoreMap> sentences = document.get(CoreAnnotations.SentencesAnnotation.class);
        for (CoreMap sentence : sentences) {
          // Get the parse tree for each sentence
          Tree parseTree = sentence.get(TreeAnnotations.TreeAnnotation.class);
          // Do something interesting with the parse tree!
          System.out.println(parseTree);
        }

      }
    }
    ```

 2. **Simple CoreNLP**
    ```java
    public class CoreNLPDemo {
      public static void main(String[] args) {
        String text = "The quick brown fox jumped over the lazy dog");  // your text here!
        Document document = new Document(text);  // implicitly runs tokenizer
        for (Sentence sentence : document.sentences()) {
          Tree parseTree = sentence.parse();  // implicitly runs parser
          // Do something with your parse tree!
          System.out.println(parseTree);
        }
      } 
    }
    ```

3. **CoreNLP Server**
  
   Start the server with the following (setting your classpath appropriately):

       java -mx4g -cp "*" edu.stanford.nlp.pipeline.StanfordCoreNLPServer [port] [timeout]

   Get a JSON-formatted output for a given set of annotators, and print it to standard out:

        wget --post-data 'The quick brown fox jumped over the lazy dog.' 'localhost:9000/?properties={"annotators":"tokenize,ssplit,parse","outputFormat":"json"}' -O -

   To get our parse tree from the JSON, we can navigate the JSON to `sentences[i].parse`.


