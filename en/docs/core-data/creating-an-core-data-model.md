---
title: "Creating an Core Data Model"
slug: "creating-an-core-data-model"
draft: false
images: []
weight: 9992
type: docs
toc: true
---


Attribute types include:
Undefined, Integer 16, Integer 32,
    Integer 64,
    Decimal, 
    Double, 
    Float,
    String, 
    Boolean, 
    Date,
    Binary, 
    Data, or 
    Transformable



When defining an `Entity` as abstract you won't be creating any instances of that entity. For example a Person would be abstract and a Employee or Customer would be a concrete subentities. 

`Transient` attributes are properties that you define as part of the model, but which are not saved to the persistent store as part of an entity instance’s data. Core Data does track changes you make to transient properties, so they are recorded for undo operations. You use transient properties for a variety of purposes, including keeping calculated values and derived values.

The `Destination` field defines what object (or objects) are returned when the relationship is accessed in code.

The `Inverse` field defines the other half of a relationship. Because each relationship is defined from one direction, this field joins two relationships together to create a fully bidirectional relationship.


[Source: Core Data Programming Guide][1] 


  [1]: https://developer.apple.com/library/ios/documentation/Cocoa/Conceptual/CoreData/KeyConcepts.html#//apple_ref/doc/uid/TP40001075-CH30-SW1

## Adding an Entity to Core Data Model
 1. First it is important to understand that the Core Data Model is the
    `*.xcdatamodeld` file. You will notice you have not entities. You
    will have to create one yourself. At the bottom of Xcode you will
    notice a button that says "Add Entity" click it and you will have a
    new entity in the navigator area for you to work with on the project.

[![Initial .xcdatamodeld][1]][1]


  [1]: http://i.stack.imgur.com/L0J3J.png

## Adding Attributes to Entity

Under the attributes section you add the attributes to your model. This button is a plus located at the bottom of the section. You can add any attributes that are relevant to your app. You have several options of types to choose from ranging from Booleans to Dates and more. The Inspector panel also has several options. 

[![Attributes][1]][1]

This is the Inspector Panel which allows you to add properties to the `Attribute` for example if you were adding an email you could provide a regex string `".+@([A-Za-z0-9-]+\\.)+[A-Za-z]{2}[A-Za-z]*"` to prevent postal addresses from being added to your email Attribute. Validation could allow for a min and max character for a phone number. 

[![Inspector Panel][2]][2]


  [1]: http://i.stack.imgur.com/ReQda.png
  [2]: http://i.stack.imgur.com/EWxY3.png

## Adding Relationships to Core Data Model
Relationships are relationship between entities that can be one-to-one or one-to-many. Creating a relationship is not needed to use Core Data. 


[![Adding a relationship][1]][1]

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/YyjEe.png
  [2]: http://i.stack.imgur.com/Y64rO.png

