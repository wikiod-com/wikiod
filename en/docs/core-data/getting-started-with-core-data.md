---
title: "Getting started with core-data"
slug: "getting-started-with-core-data"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating Your First Model

 - Select the `.xcdatamodeld` file. You will notice you have no
   entities. You will have to create one yourself. At the bottom of
   Xcode you will notice a button that says "Add Entity" click it and
   you will have a new entity for you to work with on the project.

[![Data Model][3]][3]

 - In this step there are a few points to mention. First is that you changed your entity name here the entity is given the name `Person`. Under the attributes section you add the attributes to your model. This button is a plus located at the bottom of the section. You can add any attributes that are relevant to your app. For example if you are making a contact book app, you don't need to make the model of a Person contain an attribute of `Arms` with type `Boolean`. You should keep it related to your app. For example adding a `telephone` attribute with type Integer or if you like String. You have several options of types to choose from ranging from `Booleans` to `Dates` and more. 

[![Creating your model][4]][4]

 - After you are satisfied with your model, you can create the NSManagedObject subclass. 

[![Create NSManagedObjectSubclass][5]][5]

[![Selecting Data Model][6]][6]

[![Selecting Entity to manage][7]][7]

 - Here we can see two important things. First, that Person (same as your entity name) is a subclass of NSManagedObject. This is important for your Core Data Stack. Second, you have create two files, a class `Person.swift` and an extension of `Person` called `Person+CoreDataProperites.swift`.

- All additions should be done in `Person.swift`, since if you ever change your model and re-run the class generator, it will overwrite everything in `Person+CoreDataProperties.swift`.

[![Person.swift][8]][8]

[![Person+CoreDataProperites.swift][9]][9]

  [1]: http://i.stack.imgur.com/90mFB.png
  [2]: http://i.stack.imgur.com/P22vR.png
  [3]: http://i.stack.imgur.com/L0J3J.png
  [4]: http://i.stack.imgur.com/ReQda.png
  [5]: http://i.stack.imgur.com/jM5ln.png
  [6]: http://i.stack.imgur.com/sW5Kp.png
  [7]: http://i.stack.imgur.com/VadVy.png
  [8]: http://i.stack.imgur.com/sWcR0.png
  [9]: http://i.stack.imgur.com/nG43y.png

## Creating the project


