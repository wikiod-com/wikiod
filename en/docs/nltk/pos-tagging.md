---
title: "POS Tagging"
slug: "pos-tagging"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Part of speech tagging creates __tuples__ of words and parts of speech. It labels words in a sentence as nouns, adjectives, verbs,etc. It can also label by tense, and more.
These tags mean whatever they meant in your original training data. You are free to invent your own tags in your training data, as long as you are consistent in their usage.
Training data generally takes a lot of work to create, so a pre-existing corpus is typically used. These usually use the Penn Treebank and Brown Corpus.

# Important points to note
* The variable <b>word</b> is a list of tokens.
* Even though item **i** in the list **word** is a token, tagging single token will tag each letter of the word.
* nltk.tag.pos_tag_ accept a 
    + **list of tokens** -- then separate and tags its elements or
    + **list of string**
* You can not get the tag for one word, instead you can put it within a list.  
* [POS tag](http://www.nltk.org/api/nltk.tag.html#nltk.tag.pos_tag)

## Basic Example
    import nltk
    from nltk.tokenize import sent_tokenize, word_tokenize
    text = 'We saw the yellow dog'
    word = word_tokenize(text)
    tag1 = nltk.pos_tag(word)
    print(tag1)

