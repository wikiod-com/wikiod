---
title: "Moshi"
slug: "moshi"
draft: false
images: []
weight: 9941
type: docs
toc: true
---


Moshi is a modern JSON library for Android and Java. It makes it easy to parse JSON into Java objects and Java back into JSON.

Don't forget, always read the [README][1]!


  [1]: https://github.com/square/moshi

## JSON into Java
    String json = ...;
    
    Moshi moshi = new Moshi.Builder().build();
    JsonAdapter<BlackjackHand> jsonAdapter = moshi.adapter(BlackjackHand.class);
    
    BlackjackHand blackjackHand = jsonAdapter.fromJson(json);
    System.out.println(blackjackHand);

## serialize Java objects as JSON
    BlackjackHand blackjackHand = new BlackjackHand(
        new Card('6', SPADES),
        Arrays.asList(new Card('4', CLUBS), new Card('A', HEARTS)));
    
    Moshi moshi = new Moshi.Builder().build();
    JsonAdapter<BlackjackHand> jsonAdapter = moshi.adapter(BlackjackHand.class);
    
    String json = jsonAdapter.toJson(blackjackHand);
    System.out.println(json);

## Built in Type Adapters
Moshi has built-in support for reading and writing Java’s core data types:

 - Primitives (int, float, char...) and their boxed counterparts
   (Integer, Float, Character...). 
 - Arrays
 - Collections
 - Lists
 - Sets
 - Maps Strings Enums

It supports your model classes by writing them out field-by-field. In the example above Moshi uses these classes:

    class BlackjackHand {
      public final Card hidden_card;
      public final List<Card> visible_cards;
      ...
    }
    
    class Card {
      public final char rank;
      public final Suit suit;
      ...
    }
    
    enum Suit {
      CLUBS, DIAMONDS, HEARTS, SPADES;
    }
    to read and write this JSON:
    
    {
      "hidden_card": {
        "rank": "6",
        "suit": "SPADES"
      },
      "visible_cards": [
        {
          "rank": "4",
          "suit": "CLUBS"
        },
        {
          "rank": "A",
          "suit": "HEARTS"
        }
      ]
    }

