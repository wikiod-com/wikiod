---
title: "Getting started with random"
slug: "getting-started-with-random"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting random set up or installed.

## Fisher-Yates shuffle
Also known as the Knuth shuffle and the Durstenfeld-Fisher-Yates shuffle. This shuffle takes an array of `n` elements and shuffles it. The algorithm is truly random in that, after shuffling, each permutation of the array is equally likely.

In java:


    public static void shuffle(E[] deck) {

        //From the end, swap each card with a random card from the unswapped portion.
        for(int i = deck.length - 1; i > 0; i--)
        {
            //Pick an element from [0,i], inclusive.
            int chosenCard = (int) (Math.random() * (i + 1));

            E temp = deck[i];
            deck[i] = deck[chosenCard];
            deck[chosenCard] = temp;
        }
    }
Please note: it is necessary that the replacement element come from [0,i] inclusive and not [0,i) exclusive: Otherwise, permutations of the array where elements remain in place are impossible, which is not truly random.

Assuming assuming random numbers take O(1) to generate, the algorithm operates in place and takes O(n) time and space. An array shuffled this way can be used to retrieve non-repeating elements in O(1) amortized time per element.

    E[] deck;
    int drawIndex;

    //Elements are taken from an index that advances.
    public E drawUniqueCard()
    {
        //Once all cards have been drawn, reshuffle the deck and draw from the top.
        if(drawIndex == deck.length)
        {
            shuffle(deck);
            drawIndex = 0;
        }
        //Pull the next card off the deck.
        return deck[drawIndex++];
    }

