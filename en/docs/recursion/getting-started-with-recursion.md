---
title: "Getting started with Recursion"
slug: "getting-started-with-recursion"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Checking for Palindromes with Recursion in C++
    // Checks a string to see if it is a palindrome
    bool function IsPalindrome(string input) {
        if (input.size() <= 1) {
            return true;
        }
        else if (input[0] == input[input.size() - 1]) {
            return IsPalindrome(input.substr(1,input.size() - 2));
        }
        else {
            return false;
        }
    }

## Fibonacci Sequence Using Recursion in Javascript
    // Returns the nth number in the Fibonacci sequence
    function fibonacci(n) {
        if (n === 1 || n === 0) {
            return 1;
        } else {
            return fibonacci(n - 1) + fibonacci(n - 2);
        }
    }

## Create A Hierarchy Structure Using Recursion  [JavaScript]
```
var directories = [
  {name: 'users' , parent : null },
  {name: 'distalx' , parent : 'users' },
  {name: 'guest' , parent : 'users' },
  {name: 'shared' , parent : 'users' },
  {name: 'documents' , parent : 'distalx' },
  {name: 'music' , parent : 'distalx' },
  {name: 'desktop' , parent : 'distalx' },
  {name: 'javascript' , parent : 'documents' },
  {name: 'funjs' , parent : 'documents' },
  {name: 'functions' , parent : 'documents' }
]


var sortDirectories= function(directories, parent){
  let node = [];
  directories
  .filter(function(d){ return d.parent === parent})
  .forEach(function(d){
    var cd = d;
    cd.child = sortDirectories(directories, d.name);
    return node.push(cd);
  })
  return node;
}

var results = sortDirectories(directories, null);
JSON.stringify(results, null, ' ');

```
## output

```

[{
  "name": "users",
  "parent": null,
  "child": [
      { "name": "distalx",
        "parent": "users",
        "child": [
            { "name": "documents",
              "parent": "distalx",
              "child": [
                  { "name": "javascript",
                    "parent": "documents",
                    "child": []
                  },
                  { "name": "funjs",
                    "parent": "documents",
                    "child": []
                  },
                  { "name": "functions",
                    "parent": "documents",
                    "child": []
                  }
              ]
            },
            { "name": "music",
              "parent": "distalx",
              "child": []
            },
            { "name": "desktop",
              "parent": "distalx",
              "child": []
            }
        ]
      },
      {
        "name": "guest",
        "parent": "users",
        "child": []
      },
      {
        "name": "shared",
        "parent": "users",
        "child": []
      }
  ]
}]

## Searching For A Value In A Binary Search Tree With Recursion in C/C++
Structure for Binary Search Tree (BST) nodes:

    struct node {
        int data;
        node * left;
        node * right;
    }

Search Algorithm

    // Check if a value exists in the tree
    bool BSTSearch(node * current, int value)
    {
        if (current->data == value)
        {
            return true;
        }
        else if (current->data < value)
        {
            if (current->left == NULL)
            {
                return false;
            }
            else
            {
                return BSTSearch(current->left, value);
            }
        }
        else if (current->data > value)
        {
            if (current->right == NULL)
            {
                return false;
            }
            else
            {
                return BSTSearch(current->right, value);
            }
        }
    }

## Get all Combinations of Elements from Arrays in Javascript
    function getCombinations(params, combinationsResults){
        if(params.length == 0) return combinationsResults;
        var head = params[0];
        var tail = params.slice(1);
        var combinationsResultsCurrent = [];
        if(Array.isArray(head)){
            _.uniq(head).forEach(function(item){
                if(combinationsResults.length == 0){
                    combinationsResultsCurrent.push(item);
                } else {
                    combinationsResults.forEach(function(previousResultItem){
                        combinationsResultsCurrent.push(previousResultItem.concat([item]));
                    });
                }
            });
        } else {
            if(combinationsResults.length == 0){
                combinationsResultsCurrent.push(head);
            } else {
                combinationsResults.forEach(function(previousResultItem){
                    combinationsResultsCurrent.push([previousResultItem].concat([head]));
                });
            }
        }
        return getCombinations(tail, combinationsResultsCurrent);

example:

Suppose we have a query with IN caluses:

    SELECT * FROM custom_table WHERE user_id = 'user1' AND location IN ('home', 'work', AND date IN ('2017-01-10', '2017-01-11'))

AND would like to get all combinations of parameters to generate queries without IN conditions:

    SELECT * FROM custom_table WHERE user_id = [value for user_id] AND location = [value for possible location] AND date = [value for possible date]



    

In order to get all possible combinations of parameters for equivalent queries, we can run the function above:

    var params = ['user1', ['home', 'work'], ['2017-01-10', '2017-01-11']];

    



