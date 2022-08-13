---
title: "Queries"
slug: "queries"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## BooleanQuery
<!-- language-all: lang-java -->

BooleanQuery is used to combine other queries.

They can be combined using three BooleanClause.Occur parameters:

 - `BooleanClause.Occur.MUST` - The subquery must be matched.
 - `BooleanClause.Occur.SHOULD` - The subquery may not be matched, but will be scored more highly if it is.  If there are no MUST clauses, then at least one SHOULD clause must be matched.
 - `BooleanClause.Occur.MUST_NOT` - The subquery must not match the document.

In this example, a document will match if it has "important", but *not* "forbidden", and will get a higher score if it also has "helpful".

    Query importantQuery = new TermQuery(new Term("fieldname", "important"));
    Query optionalQuery = new TermQuery(new Term("fieldname", "helpful"));
    Query forbidQuery = new TermQuery(new Term("fieldname", "forbidden"));
    BooleanQuery query = new BooleanQuery.Builder()
            .add(importantQuery, BooleanClause.Occur.MUST) 
            .add(optionalQuery, BooleanClause.Occur.SHOULD) 
            .add(forbidQuery, BooleanClause.Occur.MUST_NOT) 
            .build();

Alternatively, you can also specify the minimum number of clauses that must be matched:

    Query query1 = new TermQuery(new Term("fieldname", "one"));
    Query query2 = new TermQuery(new Term("fieldname", "two"));
    Query query3 = new TermQuery(new Term("fieldname", "three"));
    BooleanQuery query = new BooleanQuery.Builder()
            .add(query1, BooleanClause.Occur.SHOULD) 
            .add(query2, BooleanClause.Occur.SHOULD) 
            .add(query3, BooleanClause.Occur.SHOULD)
            .setMinimumNumberShouldMatch(2)
            .build();

Gotcha:  Clauses with `BooleanClause.Occur.MUST_NOT` *do not* match everything else, they only eliminate matches.  Your BooleanQuery must have at least one `MUST` or `SHOULD` clause, or it will match nothing.  This, for example, will **NOT** work:

    //***This does NOT work!***
    Query forbidQuery = new TermQuery(new Term("fieldname", "forbidden"));
    BooleanQuery getEverythingElseQuery = new BooleanQuery.Builder()
            .add(forbidQuery, BooleanClause.Occur.MUST_NOT) 
            .build();

## Boosting queries
<!-- language-all: lang-java -->

A query can be boosted to increase it's score relative to other subqueries.  This is done by wrapping it with a BoostQuery

    Query lessRelevantQuery = new TermQuery(new Term("fieldname", "ipsum"));
    //Five times as interesting
    Query highlyRelevantQuery = new BoostQuery(
             new TermQuery(new Term("fieldname", "lorem")), 
             5.0f);
    BooleanQuery query = new BooleanQuery.Builder()
            .add(lessRelevantQuery, BooleanClause.Occur.SHOULD) 
            .add(highlyRelevantQuery, BooleanClause.Occur.SHOULD) 
            .build();

## PhraseQuery
<!-- language-all: lang-java -->

PhraseQuery is used to search for a sequence of terms.  The following matches the phrase "Hello World" (after being indexed with `StandardAnalyzer`)

    Query query = new PhraseQuery.Builder()
            .add(new Term("text", "hello"))
            .add(new Term("text", "world"))
            .build();

PhraseQuery can also handle "slop", or extra terms within a query, by setting a maximum [edit distance](https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance) with `setSlop`.  This will match "Lorem ipsum sit amet dolor":

    Query query = new PhraseQuery.Builder()
            .add(new Term("text", "lorem"))
            .add(new Term("text", "amet"))
            .setSlop(2)
            .build();

You can also set exact position increments:

    Query query = new PhraseQuery.Builder()
            .add(new Term("text", "lorem"), 0)
            .add(new Term("text", "sit"), 2)
            .add(new Term("text", "dolor"), 4)
            .build();


## DisjunctionMaxQuery
<!-- language-all: lang-java -->

This combines queries such that the best (that is, highest-scoring) match of it's subqueries contributes to the final score.  

    List<Query> disjuncts = new ArrayList<Query>();
    disjuncts.add(new TermQuery(new Term("fieldname", "hello")));
    disjuncts.add(new TermQuery(new Term("fieldname", "world")));
    Query query = new DisjunctionMaxQuery(disjuncts, 0.0f);

The second argument to the `DisjunctionMaxQuery` constructor is a tiebreaker value, which, when non-zero, allows non-maximal matches to make some small contribution to score, in order to break ties.  It should generally be small (on the order of 0.1).

