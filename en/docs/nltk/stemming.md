---
title: "Stemming"
slug: "stemming"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Stemming is a sort of normalizing method. Many variations of words carry the same meaning, other than when tense is involved. The reason why we stem is to shorten the lookup, and normalize sentences. Basically, it is finding the root of words after removing verb and tense part from it. One of the most popular stemming algorithms is the Porter stemmer, which has been around since 1979.

## Porter stemmer
1. Import `PorterStemmer` and initialize

        from nltk.stem import PorterStemmer
        from nltk.tokenize import word_tokenize
        ps = PorterStemmer()

2. Stem a list of words

        example_words = ["python","pythoner","pythoning","pythoned","pythonly"]
    
        for w in example_words:
            print(ps.stem(w))

    Result:

        python
        python
        python
        python
        pythonli

3. Stem a sentence after tokenizing it.

        new_text = "It is important to by very pythonly while you are pythoning with python. All pythoners have pythoned poorly at least once."
    
        word_tokens = word_tokenize(new_text)
        for w in word_tokens:
            print(ps.stem(w))   # Passing word tokens into stem method of Porter Stemmer

    Result:

        It
        is
        import
        to
        by
        veri
        pythonli
        while
        you
        are
        python
        with
        python
        .
        all
        python
        have
        python
        poorli
        at
        least
        onc
        .



