---
title: "Getting started with stanford-nlp"
slug: "getting-started-with-stanford-nlp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic Setup from GitHub
This example goes over how to set up CoreNLP from the [GitHub repo](https://github.com/stanfordnlp/CoreNLP). The GitHub code has newer features than the official release, but may be unstable. This example will take you through downloading, building, and running a simple command-line invocation of CoreNLP.


**Prerequisites:**

* Java 8 or newer.
* [Apache Ant](http://ant.apache.org/)
* Git
* For the example: Bash or similar shell, and wget or curl

**Steps:**

1. Clone the CoreNLP Git repository:

       git clone git@github.com:stanfordnlp/CoreNLP.git

2. Enter the CoreNLP directory:

       cd CoreNLP

3. Build the project into a self-contained jar file. The easiest way to do this is with:

       ant jar

4. Download the latest models.

       wget http://nlp.stanford.edu/software/stanford-corenlp-models-current.jar

   Or using curl (what you get by default on macOS):

       curl -O http://nlp.stanford.edu/software/stanford-corenlp-models-current.jar

5. Set up your classpath. If you're using an IDE, you should set the classpath in your IDE. 

       export CLASSPATH="$CLASSPATH:javanlp-core.jar:stanford-corenlp-models-current.jar";
       for file in `find lib -name "*.jar"`; do export CLASSPATH="$CLASSPATH:`realpath $file`"; done

    If you'll be using CoreNLP frequently, this is a useful line to have in your `~/.bashrc` (or equivalent) file, replacing the directory `/path/to/corenlp/` with the appropriate path to where you unzipped CoreNLP (3 replacements):

       export CLASSPATH="$CLASSPATH:/path/to/corenlp/javanlp-core.jar:/path/to/corenlp/stanford-corenlp-models-current.jar";
       for file in `find /path/to/corenlp/lib -name "*.jar"`; do export CLASSPATH="$CLASSPATH:`realpath $file`"; don

6. Try it out! For example, the following will make a simple text file to annotate, and run CoreNLP over this file. The output will be saved to `input.txt.out` as a JSON file. Note that CoreNLP requires quite a bit of memory. You should give it at least 2GB (`-mx2g`) in most cases.

       echo "the quick brown fox jumped over the lazy dog" > input.txt
       java -mx2g edu.stanford.nlp.pipeline.StanfordCoreNLP -outputFormat json -file input.txt


## Basic Setup from Official Release
This example goes over how to set up CoreNLP from the latest official release. This example will take you through downloading the package, and running a simple command-line invocation of CoreNLP.

**Prerequisites:**

* Java JVM 8. The command `java -version` should complete successfully with a line like: *java version "1.8.0_92"*.
* Zip tool
* For the example: Bash or similar shell, and wget

**Steps:**

1. Download the CoreNLP zip file at: http://stanfordnlp.github.io/CoreNLP/index.html#download:
   
       wget http://nlp.stanford.edu/software/stanford-corenlp-full-2015-12-09.zip
2. Unzip the release:
        
       unzip stanford-corenlp-full-2015-12-09.zip

3. Enter the newly unzipped directory:

       cd stanford-corenlp-full-2015-12-09

4. Set up your classpath. If you're using an IDE, you should set the classpath in your IDE. 

       for file in `find . -name "*.jar"`; do export CLASSPATH="$CLASSPATH:`realpath $file`"; done

    If you'll be using CoreNLP frequently, this is a useful line to have in your `~/.bashrc` (or equivalent) file, replacing the directory `/path/to/corenlp/` with the appropriate path to where you unzipped CoreNLP:

       for file in `find /path/to/corenlp/ -name "*.jar"`; do export CLASSPATH="$CLASSPATH:`realpath $file`"; done

5. Try it out! For example, the following will make a simple text file to annotate, and run CoreNLP over this file. The output will be saved to `input.txt.out` as a JSON file. Note that CoreNLP requires quite a bit of memory. You should give it at least 2GB (`-mx2g`) in most cases.

       echo "the quick brown fox jumped over the lazy dog" > input.txt
       java -mx2g edu.stanford.nlp.pipeline.StanfordCoreNLP -outputFormat json -file input.txt


