---
title: "Analysis"
slug: "analysis"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Creating a custom analyzer
<!-- language-all: lang-java -->

Most analysis customization is in the `createComponents` class, where the Tokenizer and TokenFilters are defined.

CharFilters can be added in the `initReader` method.

    Analyzer analyzer = new Analyzer() {
        @Override
        protected Reader initReader(String fieldName, Reader reader) {
            return new HTMLStripCharFilter(reader);
        }
           
        @Override
        protected TokenStreamComponents createComponents(String fieldName) {
            Tokenizer tokenizer = new StandardTokenizer();
            TokenStream stream = new StandardFilter(tokenizer);
            //Order matters!  If LowerCaseFilter and StopFilter were swapped here, StopFilter's
            //matching would be case sensitive, so "the" would be eliminated, but not "The"
            stream = new LowerCaseFilter(stream);
            stream = new StopFilter(stream, StopAnalyzer.ENGLISH_STOP_WORDS_SET);
            return new TokenStreamComponents(tokenizer, stream);
        }
    };

## Iterating manually through analyzed tokens
<!-- language: lang-java -->

    TokenStream stream = myAnalyzer.tokenStream("myField", textToAnalyze);
    stream.addAttribute(CharTermAttribute.class);
    stream.reset();
    while(stream.incrementToken()) {
        CharTermAttribute token = stream.getAttribute(CharTermAttribute.class);
        System.out.println(token.toString());
    }
    
    stream.close();

A number of [Attributes](https://lucene.apache.org/core/6_1_0/core/org/apache/lucene/util/Attribute.html) are available.  The most common is `CharTermAttribute`, which is used to get the analyzed term as a String.

