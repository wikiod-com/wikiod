---
title: "N-GRAMS"
slug: "n-grams"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

N-GRAMs are statistical models that predict the next word in the sentence by using the previous n-1 words. This type of statistical models that uses word sequences are also called Language Models. For instance we have a sentence "I can't read without my reading _____", we can tell that the next most likely word would be "glasses". N-GRAMS predicts the next word in the sequence by using the conditional probability of the next word. N-GRAM model is very essential in speech and language processing.

## Syntax
 - The conditional probability of the next most likely word can be obtained by using a big corpus(Managed Collection of text or speech data), it is all bout counting things(words) from the corpus. The goal is to find P(w|h), which the probability of next word in the sequence given some history h.
 - The Concept of the N-GRAM model is that instead of computing the probability of a word given its entire history, it shortens the history to previous few words. When we use only a single previous word to predict the next word it is called a Bi-GRAM model. For Example, we have P(glasses|reading), the probability of the word "glasses" given the previous word "reading" is computed as:(Refer to the example)

N-GRAM models are very important when we have to identify words in a noisy and ambiguous input. N-GRAM models are used in:

- Speech Recognition
- Hand Writing Recognition 
- Spell Correction
- Machine Translation
- many other applications

You can read more about N-GRAM models in:

- Speech and Language Processing
Book by Daniel Jurafsky and James H. Martin

## Computing the Conditional Probability
 



> P( *glasses* | *reading* ) = Count( *reading glasses* ) / Count( *reading* )

We count the sequences `reading glasses` and `glasses` from corpus and compute the probability.

