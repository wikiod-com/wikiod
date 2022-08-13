---
title: "Working with language tags"
slug: "working-with-language-tags"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Getting a language tag from a literal
Literals in RDF may be language tagged strings.  These literals have a text part as well as a language tag.  For instance, the literal **"color"@en** has the text part **"color"** and the language tag **"en"**.  In a SPARQL query, use the [**lang**][1] function to get the language of a literal with a language tag.  If a literal does not have a language tag, then **lang** returns the empty string, **""**.


  [1]: https://www.w3.org/TR/sparql11-query/#func-lang

## Comparing language tags
The SPARQL function [**langMatches**][1] can be used to compare the language tags of RDF literals in SPARQL queries.  The simple equality operator, **=**, can be used to compare exact string matches, but will not correctly consider regional variants.  For instance, the four possible values of **?str** in:

<!-- language-all: sql -->

    values ?str { "color"@en-US "color"@en "colour"@en "colour"@en-GB }

are all English language strings, but two of these have regional specifications.  This means that 

    select ?str { 
      values ?str { "color"@en-US "color"@en "colour"@en "colour"@en-GB }
      filter (lang(?str) = 'en')
    }

will return only two results, since only two of the values of **?str** have **"en"** as a language tag.  However, 

    select ?str { 
      values ?str { "color"@en-US "color"@en "colour"@en "colour"@en-GB }
      filter langMatches(lang(?str), 'en')
    }

will return all four values.

  [1]: https://www.w3.org/TR/sparql11-query/#func-langMatches

