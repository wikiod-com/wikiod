---
title: "Deleting Documents using a Multi-Term Query"
slug: "deleting-documents-using-a-multi-term-query"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Deleting documents from a Lucene index is easy when you have a primary key field in your document (like in traditional SQL databases).

However, sometimes deleting a number of documents based on multiple fields in the document is what you need. The Lucene API allows you to achieve this by specifying a query to use for deletion.

To do this, pick the right Analyzer, construct the query, pass the query to the indexWriter to delete the documents.

## Syntax
1. indexWriter.deleteDocuments(multiTermQuery);
2. Query multiTermQuery = new QueryParser("", analyzer).parse("field_name1:\"field value 1\" AND field_name2:\"field value 2\"");
3. BooleanQuery multiTermQuery = new BooleanQuery();
    multiTermQuery.add(new TermQuery(new Term("field_name1", "field value 1")), BooleanClause.Occur.MUST);
    multiTermQuery.add(new TermQuery(new Term("field_name2", "field value 2")), BooleanClause.Occur.MUST);

# Caveats with the Choice of Analyzer
It's not immediately obvious, but the analyzer that you are using makes a huge difference to the way your query is run. This is because the StandardAnalyzer filters out common English words like "the" and "a". You might want to pick a different analyzer (like KeywordAnalyzer) so that it matches exactly. This obviously depends on you application of Lucene of course.

## Choice of Analyzer
First of all, watch out which analyzer you are using. I was stumped for a while only to realise that the StandardAnalyzer filters out common words like 'the' and 'a'. This is a problem when your field has the value 'A'. You might want to consider the KeywordAnalyzer:

[See this post around the analyzer.][1]

    // Create an analyzer:
    // NOTE: We want the keyword analyzer so that it doesn't strip or alter any terms:
    // In our example, the Standard Analyzer removes the term 'A' because it is a common English word.
    // http://stackoverflow.com/a/9071806/231860
    KeywordAnalyzer analyzer = new KeywordAnalyzer();


  [1]: http://stackoverflow.com/a/9071806/231860

## Query Parser
Next, you can either create your query using the QueryParser:

[See this post around overriding the default operator.][2]
 
    // Create a query parser without a default field in this example (the first argument):
    QueryParser queryParser = new QueryParser("", analyzer);

    // Optionally, set the default operator to be AND (we leave it the default OR):
    // http://stackoverflow.com/a/9084178/231860
    // queryParser.setDefaultOperator(QueryParser.Operator.AND);

    // Parse the query:
    Query multiTermQuery = queryParser.parse("field_name1:\"field value 1\" AND field_name2:\"field value 2\"");

[2]: http://stackoverflow.com/a/9084178/231860

## Query API
Or you can achieve the same by constructing the query yourself using their API:

[See this tutorial around creating the BooleanQuery.][3]

    BooleanQuery multiTermQuery = new BooleanQuery();
    multiTermQuery.add(new TermQuery(new Term("field_name1", "field value 1")), BooleanClause.Occur.MUST);
    multiTermQuery.add(new TermQuery(new Term("field_name2", "field value 2")), BooleanClause.Occur.MUST);

[3]: http://www.avajava.com/tutorials/lessons/how-do-i-combine-queries-with-a-boolean-query.html

## Delete the Documents that Match the Query
Then we finally pass the query to the writer to delete documents that match the query:

[See the answer to this question.][1]

[See the API here][2]

    // Remove the document by using a multi key query:
    // http://www.avajava.com/tutorials/lessons/how-do-i-combine-queries-with-a-boolean-query.html
    indexWriter.deleteDocuments(multiTermQuery);


  [1]: http://stackoverflow.com/a/4851313/231860
  [2]: https://lucene.apache.org/core/4_6_0/core/org/apache/lucene/index/IndexWriter.html

