---
title: "Sentence boundary detection in Python"
slug: "sentence-boundary-detection-in-python"
draft: false
images: []
weight: 9897
type: docs
toc: true
---

## With Stanford CoreNLP, from Python
You first need to run a [Stanford CoreNLP](http://stanfordnlp.github.io/CoreNLP/) server:

    java -mx4g -cp "*" edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 9000 -timeout 50000

Here is a code snippet showing how to pass data to the Stanford CoreNLP server, using the `pycorenlp` Python package.

    from pycorenlp import StanfordCoreNLP
    import pprint
    
    if __name__ == '__main__':
        nlp = StanfordCoreNLP('http://localhost:9000')
        fp = open("long_text.txt")
        text = fp.read()
        output = nlp.annotate(text, properties={
            'annotators': 'tokenize,ssplit,pos,depparse,parse',
            'outputFormat': 'json'
        })
        pp = pprint.PrettyPrinter(indent=4)
        pp.pprint(output)

## With python-ucto
[Ucto][1] is a rule-based tokeniser for multiple languages. It does sentence boundary detection as well. Although it is written in C++, there is a Python binding [python-ucto][2] to interface with it.

<!-- language: python -->

    import ucto 

    #Set a file to use as tokeniser rules, this one is for English, other languages are available too:
    settingsfile = "/usr/local/etc/ucto/tokconfig-en"

    #Initialise the tokeniser, options are passed as keyword arguments, defaults:
    #   lowercase=False,uppercase=False,sentenceperlineinput=False,
    #   sentenceperlineoutput=False,
    #   sentencedetection=True, paragraphdetection=True, quotedetection=False,
    #   debug=False
    tokenizer = ucto.Tokenizer(settingsfile)

    tokenizer.process("This is a sentence. This is another sentence. More sentences are better!")

    for sentence in tokenizer.sentences():
        print(sentence)



  [1]: https://languagemachines.github.io/ucto
  [2]: https://github.com/proycon/python-ucto

## Using NLTK Library
You can find more info about Python [Natural Language Toolkit][1] (NLTK) sentence level tokenizer on their [wiki][2].

From your command line:

    $ python
    >>> import nltk
    >>> sent_tokenizer = nltk.tokenize.PunktSentenceTokenizer()
    >>> text = "This is a sentence. This is another sentence. More sentences are better!"
    >>> sent_tokenizer.tokenize(text)
    Out[4]:
    ['This is a sentence.',
     'This is another sentence.',
     'More sentences are better!']


  [1]: http://www.nltk.org/
  [2]: https://github.com/nltk/nltk/wiki

