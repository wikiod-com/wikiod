---
title: "Getting started with lucene"
slug: "getting-started-with-lucene"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
This basic Lucene example creates a simple index, and searches on it.

Note:  RAMDirectory creates a memory-resident index, and is handy for experimenting and testing, but in practice most people will need to have an index stored in the file system (see [FSDirectory.open](https://lucene.apache.org/core/6_1_0/core/org/apache/lucene/store/FSDirectory.html#open-java.nio.file.Path-)).

<!-- language: lang-java -->
    import java.io.IOException;
    import org.apache.lucene.analysis.Analyzer;
    import org.apache.lucene.analysis.standard.StandardAnalyzer;
    import org.apache.lucene.document.*;
    import org.apache.lucene.index.*;
    import org.apache.lucene.queryparser.classic.*;
    import org.apache.lucene.search.*;
    import org.apache.lucene.store.*;

    public class HelloLucene {
        public static void main(String[] args) throws IOException, ParseException
        {
            //Create a new index and open a writer
            Directory dir = new RAMDirectory();
            Analyzer analyzer = new StandardAnalyzer();
            IndexWriterConfig config = new IndexWriterConfig(analyzer);
            IndexWriter writer = new IndexWriter(dir, config);
        
            //Create a document to index
            Document doc = new Document();
            doc.add(new TextField("text", "Hello World!", Field.Store.YES));
        
            //Index the document and close the writer
            System.out.println("Indexing document: " + doc);
            writer.addDocument(doc);
            writer.close();
        
            //Open an IndexSearcher
            IndexReader reader = DirectoryReader.open(dir);
            IndexSearcher searcher = new IndexSearcher(reader);
        
            //Create a query
            QueryParser parser = new QueryParser("text", analyzer);
            Query query = parser.parse("world");
        
            //Search for results of the query in the index
            System.out.println("Searching for: \"" + query + "\"");
            TopDocs results = searcher.search(query, 10);
            for (ScoreDoc result : results.scoreDocs) {
                Document resultDoc = searcher.doc(result.doc);
                System.out.println("score: " + result.score + 
                        " -- text: " + resultDoc.get("text"));
            }
            reader.close();
        }
    }


## Setup
Lucene is a Java library.  If you don't have a Java development environment set up already, see the [Java documentation](https://www.wikiod.com/java/getting-started-with-java-language).

Download the latest version of Lucene [from the Apache website](https://lucene.apache.org/core/), and unzip it.

Add the required jars to your classpath.  The following jars will be required by many projects, including the Hello World example here:

 - `core/lucene-core-6.1.0.jar`: Core Lucene functionality.
 - `core/analysis/common/lucene-analyzers-common-6.1.0.jar`: Provides a variety of analyzers, including the ubiquitous StandardAnalyzer.
 - `queryparser/lucene-queryparser-6.1.0.jar`: Provides the query parser. 

Place the code in `HelloLucene.java`. Compile it with this command:

    javac -classpath "core/*:queryparser/*" HelloLucene.java

And run it with this command:

    java -classpath ".:core/*:queryparser/*" HelloLucene

