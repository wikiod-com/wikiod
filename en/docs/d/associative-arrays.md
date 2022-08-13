---
title: "Associative Arrays"
slug: "associative-arrays"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Standard use
<!-- language: lang-d -->
 
    int[string] wordCount(string[] wordList) {
        int[string] words;
        foreach (word; wordList) {
            words[word]++;
        }
        return words;
    }
    
    void main() {
        int[string] count = wordCount(["hello", "world", "I", "say", "hello"]);
        foreach (key; count.keys) {
            writefln("%s: %s", key, count[key]);
        }
        // hello: 2
        // world: 1
        // I: 1
        // say: 1
    
        // note: the order in the foreach is unspecified
    }

## Literals
    int[string] aa0 = ["x": 5, "y": 6];  //int values, string keys
    auto aa1 = ["x": 5.0, "y": 6.0];  // double values, string keys
    string[int] aa2 = [10: "A", 11: "B"];  //string values, int keys


 




## Add key-value pairs
    int[string] aa = ["x": 5, "y": 6];
    // The value can be set by its key:
    aa["x"] = 7;
    assert(aa["x"] == 7);
    // if the key does not exist will be added
    aa["z"] = 8;
    assert(aa["z"] == 8);


## Remove key-value pairs
Let's assume an associative array `aa`:

    int[string] aa = ["x": 5, "y": 6];

Items can be removed by using `.remove()`, if key exits will be removed and `remove` returns `true`:

    assert(aa.remove("x"));

if the given key does not exist `remove` does nothing and returns `false`:

    assert(!aa.remove("z"));


## Check if a key exist
    int[string] numbers = ["a" : 10, "b" : 20];

    assert("a" in numbers);
    assert("b" in numbers);
    assert("c" in numbers);

