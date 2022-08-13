---
title: "Getting started with DOM"
slug: "getting-started-with-dom"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## HTML markup
example input:
    
    <html>
      <head>
        <title>the title</title>
        <link href='css/app.css' type='text/css' rel='stylesheet'>
        <script src='js/app.js'></script>
      </head>
      <body>
        <h1>header</h1>
        <div>
          <p>hello!</p>
        </div>
      </body>
    </html>
DOM element output:
=

                                 ------------                             
                                 | document |    <--- DOM root object.                           
                                 ------------                             
                                      |                                   
                                      |                                   
                                 ------------                             
                                 |   html   |     <--- document.documentElement                  
                                 ------------                             
                        ______________|______________                     
                        |                           |                     
                   ------------                ------------               
                   |   head   |                |   body   |               
                   ------------                ------------               
          ______________|______________             |______________       
          |             |             |             |             |       
     ------------  ------------  ------------  ------------  ------------ 
     |  title   |  |   link   |  |  script  |  |    h1    |  |   div    | 
     ------------  ------------  ------------  ------------  ------------ 
                                                                  |       
                                                                  |       
                                                             ------------ 
                                                             |    p     | 
                                                             ------------ 
All the above elements inherit from HTMLElement interface and get customized depending on specific tag

## Getting started
The DOM (Document Object Model) is the programming interface for HTML and XML documents,
it defines the logical structure of documents and the way a document is accessed and manipulated.

The main implementers of the DOM API are web browsers. Specifications are standardized by the [W3C](https://www.w3.org/DOM/) and the [WHATWG](https://dom.spec.whatwg.org/) groups, and the object model specifies the logical model for the programming interface.

The representation of DOM structure resembles a tree-like view,
where each node is an object representing a part of the markup,
depending on the type each element also inherits specific and shared functionalities.

The name "Document Object Model" was chosen because it is an "object model" in the traditional object oriented design sense:
documents are modeled using objects, and the model encompasses not only the structure of a document,
but also the behavior of a document and the objects of which it is composed.
In other words, taking the example HTML diagram, the nodes do not represent a data structure,
they represent objects, which have functions and identity.
As an object model, the Document Object Model identifies:

 - the interfaces and objects used to represent and manipulate a document
 - semantics of these interfaces and objects - including both behavior and attributes
 - the relationships and collaborations among these interfaces and objects

## Wait for DOM to be loaded


## Retrieving existing html elements
One of the most common tasks is retrieving an existing element from the DOM to manipulate. Most commonly these methods are executed on `document`, because it is the root node, but all these methods work on any HTML element in the tree. They will only return children from the node it is executed on.

## Retrieve by id

    var element = document.getElementById("logo");

`element` will contain the (only) element that has its `id` attribute set to "logo", or contains `null` if no such element exists. If multiple elements with this id exist, the document is invalid, and anything can happen.

## Retrieve by tag name

    var elements = document.getElementsByTagName("a");

`elements` will contain a *live* `HTMLCollection` (an array-like object) of all link tags in the document. This collection is in sync with the DOM, so any changes made to the DOM are reflected in this collection. The collection provides random access and has a length.

    var element = elements[0];
    //Alternative
    element = elements.item(0);

> `element` contains the first encountered HTML link element, or `null` if the index is out of bounds

    var length = elements.length;

> `length` is equal to the number of HTML link elements currently in the list. This number can change when the DOM is changed.

## Retrieve by class

    var elements = document.getElementsByClassName("recipe");

`elements` will contain a *live* `HTMLCollection` (an array-like object) of all elements where their `class` attribute includes "recipe". This collection is in sync with the DOM, so any changes made to the DOM are reflected in this collection. The collection provides random access and has a length.

    var element = elements[0];
    //Alternative
    element = elements.item(0);

> `element` contains the first encountered HTML element with this class. If there are no such elements, `element` has the value `undefined` in the first example and `null` in the second example.

    var length = elements.length;

> `length` is equal to the number of HTML elements that currently have the class "recipe". This number can change when the DOM is changed.

## Retrieve by name

    var elements = document.getElementsByName("zipcode");

`elements` will contain a *live* `NodeList` (an array-like object) of all elements with their `name` attribute set to "zipcode". This collection is in sync with the DOM, so any changes made to the DOM are reflected in this collection. The collection provides random access and has a length.

    var element = elements[0];
    //Alternative
    element = elements.item(0);

> `element` contains the first encountered HTML element with this name.

    var length = elements.length;

> `length` is equal to the number of HTML elements that currently have "zipcode" as their name attribute. This number can change when the DOM is changed.

## Use innerHTML


