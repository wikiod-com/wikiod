---
title: "Natural Language Processing"
slug: "natural-language-processing"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

NLP is a way for computers to analyze, understand, and derive meaning from human language in a smart and useful way. By utilizing NLP, developers can organize and structure knowledge to perform tasks such as automatic summarization, translation, named entity recognition, relationship extraction, sentiment analysis, speech recognition, and topic segmentation.

## Text Matching or Similarity
> One of the important areas of NLP is the matching of text objects to
> find similarities. Important applications of text matching includes
> automatic spelling correction, data de-duplication and genome analysis
> etc.
A number of text matching techniques are available depending upon the requirement. So lets have;**Levenshtein Distance**

The Levenshtein distance between two strings is defined as the minimum number of edits needed to transform one string into the other, with the allowable edit operations being insertion, deletion, or substitution of a single character. 

Following is the implementation for efficient memory computations.

    def levenshtein(s1,s2): 
       
     if len(s1) > len(s2):
        s1,s2 = s2,s1 
    distances = range(len(s1) + 1) 

    for index2,char2 in enumerate(s2):
        newDistances = [index2+1]
        for index1,char1 in enumerate(s1):
            if char1 == char2:
                newDistances.append(distances[index1]) 
            else:
                 newDistances.append(1 + min((distances[index1], distances[index1+1], newDistances[-1]))) 
                 distances = newDistances 

                 return distances[-1]

    print(levenshtein("analyze","analyse"))

