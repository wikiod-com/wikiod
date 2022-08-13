---
title: "Adding Natural Language Processing"
slug: "adding-natural-language-processing"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Bot Framework supports `Recognizers`. A recognizer is used to recognize what to do whenever a user sends the bot any message. Therefore you can design your bot to recognize intents based on the user input. The recognizer can be used with LUIS API in order to add natural language understanding for the bot.

## Syntax
- var recognizer = new builder.LUISRecognizer('Your model's URL');

- var intents = new builder.IntentDialog({recognizers: [recognizer]});

## Initializing and Adding LUISRecognizer
Once you're up with a new project with the basic template provided in the Introduction, you should be able to add a LUISRecognizer like so -

    var model = ''    // Your LUIS Endpoint link comes here
    var recognizer = new builder.LuisRecognizer(model);

Now, `recognizer` is a LUISRecognizer and can pass intents based on your defined LUIS Model. You can add the `recognizer` to your intents by

    var intents = new builder.IntentDialog({recognizers: [recognizer]});

Your bot is now capable of handling intents from LUIS. Any named intents on LUIS can be detected by using the `matches` property of `IntentDialog` class. So say, an intent named `hi` is defined in the LUIS model, to recognize the intent on the bot,

    intents.matches('hi', function(session) {
        session.send("Hey :-)");
    });

## Defining a LUIS Model with Intents
Creating a LUIS Model requires little to no programming experience. However, you need to be familiar with 2 important terms that will be used extensively.

1. **Intents** - These are how you identify functions that need to be executed when the user types in something. Eg - An intent named `Hi` will identify a function that needs to be executed whenever the user sends "Hi". Intents are uniquely named in your program/model.
2. **Entities** - These identify the nouns in a statement. Eg - "Set an alarm for 1:00 pm", here `1:00 pm` is an entity that needs to be recognized by the chat-bot to set an alarm.

**Note:** Images of the website are not provided as the front-end my change, but the core concept remains the same.

To create a new model, go to LUIS.ai and sign-in with your Microsoft Account to be taken to the app creation page. Where a blank project can be created.

**Defining Intents:**

Intents can be defined on the `Intents` tab. They identify what function you need to perform when the user enters anything.

All applications have a default `None` intent, which is activated whenever the user input matches no other intent.

To define an intent,
1. Give it a unique name relevant to the function you want to perform.
2. Once the naming is complete, you should add `utterances` to the intent. Utterances are what you want the user to send in order to activate the intent that you are defining.
Try feeding as many different `utterances` as possible in order for LUIS to associate `intents` and `utterances` properly.
3. Train your LUIS Model, by clicking the `Train` button on `Train and Test` Tab. After training the app can be tested in the panel below.
4. Finally publish your app in the `Publish App` Tab. You should now get an endpoint URL that should be put in while defining `LUISRecognizer` in your bot code.



## Adding Entities to LUIS Model
An entity is the information that your bot extracts from a particular utterance conforming to an intent.

Eg- Let `My name is John Doe` belong to an intent called `introduction`. For your bot to understand and extract the name `John Doe` from the sentence, you need to define an entity which does so. You can name the entity whatever you wish, but it is best to name it as something pertaining to what it extracts. In our example, we can call our entity `name`.

Entities can be re-used between different intents, to extract different things. So the best principle would be to make an entity that extracts only type of data and use it across different intents. Therefore, in our above example, say `Book a flight on Emirates` belongs to the intent `booking`, then the same entity, `name`, can be used to extract the flight name `emirates`.

You need to keep in mind two things before you go on defining entities - 
1. Entities should be unique per utterance in an intent. An entity cannot be used twice in the same utterance.
2. LUIS is case insensitive. This implies that everything extracted and received through entity extraction will be in lower-case. So extracting case-sensitive data through entities is probably a bad idea.

**Adding pre-built entities**

Pre-built entities are, as the name suggests, pre-built i.e. they are already configured to extract a particular type of data across the intent they are added to. An example can be the entity `number` that extracts numbers from the intent it is assigned to. The numbers can be either in numeric or alphabetical like `10` or `ten`.

For a full list of all pre-built entities, you can visit [Pre-built Entities List][1].

To add pre-built entities,
1. Go to the `entities` tab.
2. Click `Add pre-built entities` and select the entity you want to add to the model and hit save.

**Adding Custom Entities**
Custom Entities are of 4 types,
1. **Simple**: A simple entity extracts a particular data, `name` in the examples above is a simple entity.
2. **Hierarchical**: A parent entity with children entities (sub-types) which are dependent on the parent.
3. **Composite**: A group of 2 or more entities independent together.
4. **List**: An entity that recognizes words only from a given list.

*Defining Simple Entities*
1. Go the the `entities` tab.
2. Click on `Add Custom Entities`
3. Name your entity, check the required entity type and hit `Save`.

All other type of entities can be added in the same way by just changing the `Entity Type` to one of the above types. In hierarchical and composite entity types, you'll also need to give the children names along with the parent entity name. Defining List entities is a little different than the rest. 

*Defining List Entities*

After you follow the above steps to create a `List Entity` by putting th `Entity Type` as List, you'll be directed to the details page of the entity you just defined.

1. Define a canonical value. This is a standard value that the bot will receive when the user types in any of the synonyms.
2. Define synonyms to the canonical value. They will be converted to the canonical value upon being encountered by the entity.

You can also import entire lists by using an array of JSON Objects, of the form:

    [
        {
            "canonicalForm": "Hey",
            "list": [
                "Howdy",
                "Hi"
             ]
        },
        .
        .
        .
    ]

**Associating an entity with an intent**

`Pre-built` and `list` entities already have a set of values defined which can be extracted from all utterences, however, `Simple`, `Hierarchical` and `Composite` utterances need to be trained to pick up values.

This can be done by
1. Go to the `intents` tab and choose the intent you'd like to add the entity to.
2. Add an utterance with a dummy value that you would like to be extracted. Say, you can add `My name is John Doe` as an utterance.
3. Click and drag the mouse over the words you want the entity to extract. You will need to highlight `john doe` in the above example.
4. A drop-down will open with a list of all entities available in your project. Select the corresponding one as you see fit. `Name` will be the entity selected in the above example.
5. Add more utterances with different dummy values each time and all possible structures you can think of.
6. Train and publish your LUIS Model.

