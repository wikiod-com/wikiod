---
title: "Frequency Distributions"
slug: "frequency-distributions"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

This topic focuses on the use of the nltk.FreqDist() class.

## Frequency Distribution to Count the Most Common Lexical Categories
NLTK provides the FreqDist class that let's us easily calculate a frequency distribution given a list as input.

Here we are using a list of part of speech tags (POS tags) to see which lexical categories are used the most in the brown corpus.

    import nltk

    brown_tagged = nltk.corpus.brown.tagged_words()
    pos_tags = [pos_tag for _,pos_tag in brown_tagged]
    

    fd = nltk.FreqDist(pos_tags)
    print(fd.most_common(5))
    
    # Out: [('NN', 152470), ('IN', 120557), ('AT', 97959), ('JJ', 64028), ('.', 60638)]

We can see that Nouns are the most common lexical category.
Frequency Distributions can be accessed just like dictionaries. So by doing this we can calculate what percentage of the words in the brown corpus are nouns.

    print(fd['NN'] / len(pos_tags))
    # Out: 0.1313

