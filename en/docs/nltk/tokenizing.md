---
title: "Tokenizing"
slug: "tokenizing"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

It refers to the splitting of sentences and words from the body of text into sentence tokens or word tokens respectively. It is an essential part of NLP, as many modules work better (or only) with tags. For example, __pos_tag__ needs _tags_ as input and not the words, to tag them by parts of speech.

## Sentence and word tokenization from user given paragraph
    from nltk.tokenize import sent_tokenize, word_tokenize
    example_text = input("Enter the text:  ")
    
    print("Sentence Tokens:")
    print(sent_tokenize(example_text))
    
    print("Word Tokens:")
    print(word_tokenize(example_text))

