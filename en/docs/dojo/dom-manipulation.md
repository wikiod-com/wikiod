---
title: "DOM Manipulation"
slug: "dom-manipulation"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Dojo provides different functions that allows you to manipulate DOM elements such as creation, placement and destruction.

## Parameters
| **Argument**| **Type**   |    
| ------      | ------        | 
| node        | DomNode or String |






## dom-construct
This module can be used to :
 

 - Create a new element.

 - Delete an element from HTML document.

 - Place element in HTML document.

----------
## Iinitialisation ##

To be able to use the `dom-construct` module we need to load it as fallow :

    require(["dojo/dom-construct"], function(domConstruct){
        // Write code here
    });

----------

## **create()** ##


This function can be used to create an element and add it in a specific position.
It also allows you to set attributes and content.

**Usage**

    var node = domConstruct.create("div", { style: { color: "red" }}, "someId", "first"); 

----------

## **destroy()** ##

This function allows you to delete an element including it's children and content from the document. 

**Usage**

    domConstruct.destroy("someId"); 

----------

## **place()** ##

This function can be used to place nodes in a particular position in an HTML document  

**Usage**

    domConstruct.place("someNode", "refNode", "after"); 

----------

## **empty()** ##

This function can be used to delete content and all its children of a DOM element   

**Usage**

    domConstruct.empty("someId");






 

 


## dom-class
This module provides function that allows you to manipulate CSS classes of DOM elements.

Initialization
---------------

To be able to use the dom-class module we need to load it as fallow :

    require(["dojo/dom-class"], function(domClass){
        // Write code here
    });


----------

**contains()**
----------

This function checks if a node contains a specific class 

**Usage**

     if (domClass.contains("someId", "className")){
         // do something if it contains 
     } 


----------


**add()**
----------

This function allows you to add CSS Classes to a DOM node without duplication. 

**Usage**   

    domClass.add("someId", "className"); 


----------


**remove()**
----------

This function allows you to remove CSS Classes from a DOM node. 

**Usage**   

     domClass.remove("someId", "className");


----------
**replace()**
----------

This function allows you to remove classes and replace it with other classes. 

**Usage**   

     domClass.replace("someId", "addedClassName", "removedClassName");

----------
**toggle()**
----------

This function allows you to remove a class if it exist, or add it if it doesn't exist. 

**Usage**   

     domClass.toggle("someId", "className");




