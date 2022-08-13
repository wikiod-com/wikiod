---
title: "Getting started with apache-pig"
slug: "getting-started-with-apache-pig"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Linux
---------

**Requirements (r0.16.0)**

***Mandatory***

As per current `Apache-Pig` documentation it supports only `Unix` & `Windows` operating systems. 
 - Hadoop 0.23.X, 1.X or 2.X
 - Java 1.6 or Later versions installed and JAVA_HOME environment variable set to Java installation directory

***Optional***

 - Python 2.7 or more (Python UDFs)
 - Ant 1.8 (for builds)

**Download the latest Pig release**

Download the latest version of pig from http://pig.apache.org/releases.html#Download

**Installation**

    mkdir Pig
    cd Downloads/ 
    tar zxvf pig-(latest-version).tar.gz 
    tar zxvf pig-(latest-version).tar.gz 
    mv pig-(latest-version).tar.gz/* /home/Pig/

**Configuration**

After installing Apache Pig, we have to configure it.

Open the .bashrc file

    vim ~/.bashrc
In the .bashrc file, set the following variables −

    export PIG_HOME = /home/Pig
    export PATH  = PATH:/home/Pig/bin

save the file and reload bashrc again in the environment using 

    . ~/.bashrc

**Verifying Pig version**
    
    pig –version 
If the installation is successful, the above command displays the installed Pig version number. 

**Testing Pig Installation**
   
    pig -h
This should display all the possible commands associated with pig

Your pig is  now installed locally and you can run it using local parameter like

    pig -x local

**Connecting to Hadoop**

If Hadoop1.x or 2.x is Installed on the cluster and the HADOOP_HOME environment variable is setup.

you can connect pig to Hadoop by adding the line in the .bashrc like before

    export PIG_CLASSPATH = $HADOOP_HOME/conf

**Running Pig**

***Execution Modes***

You can run Pig either using the `pig` *(bin/pig)* command or by running `jar` file *(java -cp pig.jar)*

`PIG` scripts can be executed in 3 different modes:

 - **Local Mode**

        pig -x local ...

 - **Mapreduce Mode** (default mode)

        pig -x mapreduce ...
             (or)
        pig ...
 - **Tez Local Mode**

        pig -x tez ...
***Interactive Mode***

Pig can be run in interactive mode using the `Grunt` shell. Pig Latin statements and commands can be entered interactively in this shell. 

**Example**

    $ pig -x <mode> <enter>
    grunt>

`Mode` can be one of execution modes as explained in the previous section. 

***Batch Mode***

Pig can also be executed in batch mode. Here a `.pig` file containing a list of pig statements and commands is provided. 

**Example**

    $ pig -x <mode> <script.pig>
    grunt>

Similarly `Mode` can be one of execution modes as explained in the previous section. 


## Word Count Example in Pig
**Input file** 

    Mary had a little lamb
    its fleece was white as snow
    and everywhere that Mary went
    the lamb was sure to go.

**Pig Word Count Code**

    -- Load input from the file named Mary, and call the single
    -- field in the record 'line'.
    input = load 'mary' as (line);

    -- TOKENIZE splits the line into a field for each word.
    -- flatten will take the collection of records returned by
    -- TOKENIZE and produce a separate record for each one, calling the single
    -- field in the record word.
    words = foreach input generate flatten(TOKENIZE(line)) as word;
    
    -- Now group them together by each word.
    grpd = group words by word;
    
    -- Count them.
    cntd = foreach grpd generate group, COUNT(words);
    
    -- Print out the results.
    dump cntd;

**Output**

    Mary,2
    had,1
    a,1
    little,1
    lamb,2
    its,1
    fleece,1
    was,2
    white,1
    as,1
    snow,1
    and,1
    everywhere,1
    that,1
    went,1
    the,1
    sure,1
    to,1
    go,1

## What Is Pig?
Pig provides an engine for executing data flows in parallel on Hadoop. It includes a
language, Pig Latin, for expressing these data flows. Pig Latin includes operators for
many of the traditional data operations (join, sort, filter, etc.), as well as the ability for
users to develop their own functions for reading, processing, and writing data.
Pig is an Apache open source project. This means users are free to download it as source
or binary, use it for themselves, contribute to it, and—under the terms of the Apache
License—use it in their products and change it as they see fit.

