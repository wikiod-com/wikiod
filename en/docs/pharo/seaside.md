---
title: "Seaside"
slug: "seaside"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Seaside is a web framework for Pharo and other smalltalks. It is ideal for complex applications with a rich domain model.

## Droppable
A Seaside component (subclass of `WAComponent`) needs to override `#renderContentOn:`. It is a smalltalk class that can use all the normal ways of structuring an application. Here it delegates to three different methods. 

    JQDroppableFunctionalTest>>renderContentOn: html
       self renderInstructionsOn: html.
       self renderInventoryOn: html.
       self renderSelectedOn: html

As a parameter it gets a html canvas object that understands messages relevant to building up the html and javascript. It uses a fluent interface, where `#with:` is the last message send to the current canvas context.


    JQDroppableFunctionalTest>>renderInventoryOn: html
       html div class: 'inventory ui-corner-all'; with: [ 
          self colors do: [ :each |
             html div
                class: each;
                passenger: each;
                script: (html jQuery new draggable
                   revert: true) ] ]

    JQDroppableFunctionalTest>>renderSelectedOn: html
       html div
          class: 'selected ui-corner-all'; 
          script: (html jQuery new droppable
             onDrop: (html jQuery this load
                callbackDroppable: [ :event | 
                    self selected add: (event at: #draggable) ];
                html: [ :r | self renderSelectedItemsOn: r ]));
          with: [ self renderSelectedItemsOn: html ]

    JQDroppableFunctionalTest>>renderSelectedItemsOn: html
       self selected do: [ :each |
          html div 
             class: each; 
             passenger: each;
             script: (html jQuery new draggable
                onStop: (html jQuery this effect 
                   percent: 1; puff; 
                   onComplete: (html jQuery this parent load html: [ :r | 
                      self selected remove: each ifAbsent: [ ].
                      self renderSelectedItemsOn: r ]))) ]

