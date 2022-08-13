---
title: "Stop Words"
slug: "stop-words"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Stop words are the words which are mostly used as fillers and hardly have any useful meaning. We should avoid these words from taking up space in database or taking up valuable processing time. We can easily make a list of words to be used as stop words and then filter these words from the data we want to process.

## Filtering out stop words
NLTK has by default a bunch of words that it considers to be stop words. It can be accessed via the NLTK corpus with:  

    from nltk.corpus import stopwords

To check the list of stop words stored for english language :

    stop_words = set(stopwords.words("english"))
    print(stop_words)

Example to incorporate the stop_words set to remove the stop words from a given text:

    from nltk.corpus import stopwords
    from nltk.tokenize import word_tokenize
    
    example_sent = "This is a sample sentence, showing off the stop words filtration."
    stop_words = set(stopwords.words('english'))
    word_tokens = word_tokenize(example_sent)
    filtered_sentence = [w for w in word_tokens if not w in stop_words]

    filtered_sentence = []

    for w in word_tokens:
        if w not in stop_words:
            filtered_sentence.append(w)
        
    print(word_tokens)
    print(filtered_sentence)

