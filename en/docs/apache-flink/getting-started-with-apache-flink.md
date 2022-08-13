---
title: "Getting started with apache-flink"
slug: "getting-started-with-apache-flink"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Overview and requirements
# What is Flink

Like [Apache Hadoop](http://hadoop.apache.org/) and [Apache Spark](http://spark.apache.org/), Apache Flink is a community-driven open source framework for distributed Big Data Analytics. Written in Java, Flink has APIs for Scala, Java and Python, allowing for Batch and Real-Time streaming analytics. 

# Requirements

- a UNIX-like environment, such as Linux, Mac OS X or Cygwin;
- Java 6.X or later;
- [optional] Maven 3.0.4 or later.

# Stack



[![enter image description here][1]][1]

# Execution environments

> Apache Flink is a data processing system and __an alternative to Hadoop’s MapReduce component__. It comes with its _own runtime_ rather than building on top of MapReduce. As such, it can work completely independently of the Hadoop ecosystem.

The `ExecutionEnvironment` is the context in which a program is executed. There are different environments you can use, depending on your needs. 

1. _JVM environment_: Flink can run on a single Java Virtual Machine, allowing users to test and debug Flink programs directly from their IDE. When using this environment, all you need is the correct maven dependencies. 

2. _Local environment_: to be able to run a program on a running Flink instance (not from within your IDE), you need to install Flink on your machine. See [local setup](https://ci.apache.org/projects/flink/flink-docs-release-0.8/local_setup.html).

3. _Cluster environment_: running Flink in a fully distributed fashion requires a standalone or a yarn cluster. See the [cluster setup page](https://ci.apache.org/projects/flink/flink-docs-release-0.8/setup_quickstart.html) or [this slideshare](http://www.slideshare.net/sbaltagi/stepbystep-introduction-to-apache-flink) for more information.
mportant__: the `2.11` in the artifact name is the _scala version_, be sure to match the one you have on your system.

# APIs

Flink can be used for either stream or batch processing. They offer three APIs:
- __DataStream API__:  stream processing, i.e. transformations (filters, time-windows, aggregations) on unbounded flows of data. 
- __DataSet API__: batch processing, i.e. transformations on data sets.
- __Table API__: a SQL-like expression language (like dataframes in Spark) that can be embedded in both batch and streaming applications.

# Building blocks

At the most basic level, Flink is made of source(s), transformations(s) and sink(s).

[![enter image description here][2]][2]

At the most basic level, a Flink program is made up of:

- **Data source**: Incoming data that Flink processes 
- **Transformations**: The processing step, when Flink modifies incoming data
- **Data sink**: Where Flink sends data after processing 

Sources and sinks can be local/HDFS files, databases, message queues, etc. There are many third-party connectors already available, or you can easily create your own.


  [1]: https://i.stack.imgur.com/ziCa7.png
  [2]: https://i.stack.imgur.com/Zn1EI.png

## Local runtime setup
0. ensure you have java 6 or above and that the `JAVA_HOME` environment variable is set.

1. download the latest flink binary [here](https://flink.apache.org/downloads.html):

       wget flink-XXXX.tar.gz

   If you don't plan to work with Hadoop, pick the hadoop 1 version. Also, note the scala version you download, so you can add the correct maven dependencies in your programs.

2. start flink:

       tar xzvf flink-XXXX.tar.gz
       ./flink/bin/start-local.sh

   Flink is already configured to run locally.
   To ensure flink is running, you can inspect the logs in `flink/log/` or open the flink jobManager's interface running on `http://localhost:8081`.

3. stop flink:

       ./flink/bin/stop-local.sh


## Flink Environment setup
To run a flink program from your IDE(we can use either Eclipse or Intellij IDEA(preffered)), you  need two dependencies:`flink-java` / `flink-scala` and `flink-clients` (as of february 2016). These JARS can be added using Maven and SBT(if you are using scala).

 - **Maven**

<!-- language: lang-xml -->

    <dependency>
        <groupId>org.apache.flink</groupId>
        <artifactId>flink-java</artifactId>
        <version>1.1.4</version>
    </dependency>

    <dependency>
        <groupId>org.apache.flink</groupId>
        <artifactId>flink-clients_2.11</artifactId>
        <version>1.1.4</version>
    </dependency>

 - **SBT**
        name := " "
        
        version := "1.0"
        
        scalaVersion := "2.11.8"
        
        libraryDependencies ++= Seq(
          "org.apache.flink" %% "flink-scala" % "1.2.0",
          "org.apache.flink" %% "flink-clients" % "1.2.0"
        )

__important__: the `2.11` in the artifact name is the _scala version_, be sure to match the one you have on your system.

## WordCount - Table API
This example is the same as _WordCount_, but uses the Table API. See _WordCount_ for details about execution and results.

## Maven

To use the Table API, add `flink-table` as a maven dependency:

<!-- language: lang-xml -->

    <dependency>
        <groupId>org.apache.flink</groupId>
        <artifactId>flink-table_2.11</artifactId>
        <version>1.1.4</version>
    </dependency>

## The code 

<!-- language: lang-java -->

    public class WordCountTable{

        public static void main( String[] args ) throws Exception{

            // set up the execution environment
            final ExecutionEnvironment env = ExecutionEnvironment.getExecutionEnvironment();
            final BatchTableEnvironment tableEnv = TableEnvironment.getTableEnvironment( env );

            // get input data
            DataSource<String> source = env.fromElements(
                    "To be, or not to be,--that is the question:--",
                    "Whether 'tis nobler in the mind to suffer",
                    "The slings and arrows of outrageous fortune",
                    "Or to take arms against a sea of troubles"
            );

            // split the sentences into words
            FlatMapOperator<String, String> dataset = source
                    .flatMap( ( String value, Collector<String> out ) -> {
                        for( String token : value.toLowerCase().split( "\\W+" ) ){
                            if( token.length() > 0 ){
                                out.collect( token );
                            }
                        }
                    } )
                    // with lambdas, we need to tell flink what type to expect
                    .returns( String.class );

            // create a table named "words" from the dataset
            tableEnv.registerDataSet( "words", dataset, "word" );

            // word count using an sql query
            Table results = tableEnv.sql( "select word, count(*) from words group by word" );
            tableEnv.toDataSet( results, Row.class ).print();
        }
    }

_Note_: For a version using Java < 8, replace the lambda by an anonymous class:

<!-- language: lang-java -->

    FlatMapOperator<String, String> dataset = source.flatMap( new FlatMapFunction<String, String>(){
            @Override
            public void flatMap( String value, Collector<String> out ) throws Exception{
                for( String token : value.toLowerCase().split( "\\W+" ) ){
                    if( token.length() > 0 ){
                        out.collect( token );
                    }
                }
            }
        } );



## WordCount
## Maven

Add the dependencies `flink-java` and `flink-client` (as explained in the _JVM environment setup_ example).

## The code

<!-- language: lang-java -->
  
    public class WordCount{
    
        public static void main( String[] args ) throws Exception{
    
            // set up the execution environment
            final ExecutionEnvironment env = ExecutionEnvironment.getExecutionEnvironment();
    
            // input data
            // you can also use env.readTextFile(...) to get words
            DataSet<String> text = env.fromElements(
                    "To be, or not to be,--that is the question:--",
                    "Whether 'tis nobler in the mind to suffer",
                    "The slings and arrows of outrageous fortune",
                    "Or to take arms against a sea of troubles,"
            );
    
            DataSet<Tuple2<String, Integer>> counts =
                    // split up the lines in pairs (2-tuples) containing: (word,1)
                    text.flatMap( new LineSplitter() )
                            // group by the tuple field "0" and sum up tuple field "1"
                            .groupBy( 0 )
                            .aggregate( Aggregations.SUM, 1 );
    
            // emit result
            counts.print();
        }   
    }

`LineSplitter.java`:

<!-- language: lang-java -->

    public class LineSplitter implements FlatMapFunction<String, Tuple2<String, Integer>>{
    
        public void flatMap( String value, Collector<Tuple2<String, Integer>> out ){
            // normalize and split the line into words
            String[] tokens = value.toLowerCase().split( "\\W+" );

            // emit the pairs
            for( String token : tokens ){
                if( token.length() > 0 ){
                    out.collect( new Tuple2<String, Integer>( token, 1 ) );
                }
            }
        }
    }

If you use Java 8, you can replace `.flatmap(new LineSplitter())` by a lambda expression: 

<!-- language: lang-java -->

    DataSet<Tuple2<String, Integer>> counts = text
        // split up the lines in pairs (2-tuples) containing: (word,1)
        .flatMap( ( String value, Collector<Tuple2<String, Integer>> out ) -> {
            // normalize and split the line into words
            String[] tokens = value.toLowerCase().split( "\\W+" );

            // emit the pairs
            for( String token : tokens ){
                if( token.length() > 0 ){
                    out.collect( new Tuple2<>( token, 1 ) );
                }
            }
        } )
        // group by the tuple field "0" and sum up tuple field "1"
        .groupBy( 0 )
        .aggregate( Aggregations.SUM, 1 );

## Execution

__From the IDE__: simply hit _run_ in your IDE. Flink will create an environment inside the JVM. 

__From the flink command line__: to run the program using a standalone local environment, do the following:

  1. ensure flink is running (`flink/bin/start-local.sh`);
  2. create a jar file (`maven package`);
  3. use the `flink` command-line tool (in the `bin` folder of your flink installation)  to launch the program:

         flink run -c your.package.WordCount target/your-jar.jar

     The `-c` option allows you to specify the class to run. It is not necessary if the jar is executable/defines a main class.

## Result

    (a,1)
    (against,1)
    (and,1)
    (arms,1)
    (arrows,1)
    (be,2)
    (fortune,1)
    (in,1)
    (is,1)
    (mind,1)
    (nobler,1)
    (not,1)
    (of,2)
    (or,2)
    (outrageous,1)
    (question,1)
    (sea,1)
    (slings,1)
    (suffer,1)
    (take,1)
    (that,1)
    (the,3)
    (tis,1)
    (to,4)
    (troubles,1)
    (whether,1)


## WordCount - Streaming API
This example is the same as _WordCount_, but uses the Table API. See _WordCount_ for details about execution and results.

## Maven

To use the Streaming API, add `flink-streaming` as a maven dependency:

<!-- language: lang-xml -->

    <dependency>
        <groupId>org.apache.flink</groupId>
        <artifactId>flink-streaming-java_2.11</artifactId>
        <version>1.1.4</version>
    </dependency>

## The code 

<!-- language: lang-java -->

    public class WordCountStreaming{
    
        public static void main( String[] args ) throws Exception{
    
            // set up the execution environment
            StreamExecutionEnvironment env = StreamExecutionEnvironment.getExecutionEnvironment();
        
            // get input data
            DataStreamSource<String> source = env.fromElements(
                    "To be, or not to be,--that is the question:--",
                    "Whether 'tis nobler in the mind to suffer",
                    "The slings and arrows of outrageous fortune",
                    "Or to take arms against a sea of troubles"
            );
            
            source
                    // split up the lines in pairs (2-tuples) containing: (word,1)
                    .flatMap( ( String value, Collector<Tuple2<String, Integer>> out ) -> {
                        // emit the pairs
                        for( String token : value.toLowerCase().split( "\\W+" ) ){
                            if( token.length() > 0 ){
                                out.collect( new Tuple2<>( token, 1 ) );
                            }
                        }
                    } )
                    // due to type erasure, we need to specify the return type
                    .returns( TupleTypeInfo.getBasicTupleTypeInfo( String.class, Integer.class ) )
                    // group by the tuple field "0"
                    .keyBy( 0 )
                    // sum up tuple on field "1"
                    .sum( 1 )
                    // print the result
                    .print();
    
            // start the job
            env.execute();
        }
    }

