---
title: "Views"
slug: "views"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Views for people
To show you how work the views, we will assume that we want to query the document of type *people*. To do so, we will first need a design document that will hold our views.

 *Note: for the purpose of the example, we will use many views inside of 1 design document. Therefore, in a production environment, you may prefer to have 1 view per design document. The reason is that every time you update the design document, all the views are rerun (**at least for Cloudant**)*.

So at this point, I assume you know what is a design document and how it works. Our design document will look like this :

    {
        "_id":"_design/people",
        "language":"javascript"
    }

Then, inside of this document, you will have a property of views. This property holds an object containing the views. Each view has its own object that contains a **map** function and optionally, a **reduce** function. Here is how it looks if we have a view that fetches all the people from the database :

    {
        "_id":"_design/people",
        "language":"javascript",
        "views":{
            "all":{
                "map":"function(doc){if(doc.type ===\"people\")emit(doc._id);}"
            }
        }
    }

Such a view would return something like this :

    {
        "total_rows": 2,
        "offset": 0,
        "rows": [
            { "id": "people_23929319009123", "key": "people_23929319009123", "value": null },
            { "id": "people_11482871000723", "key": "people_11482871000723", "value": null }
        ]
    }

What we have made so far is the view that gives us all the people. The equivalent in SQL would be : `SELECT * FROM table WHERE type="people"`. I will explain in detail how work the map function. 

__Map function : all__

    function(doc) {
        if (doc.type === "people") emit(doc._id);
    }

First, you need to know that the map function will be executed for each document. Now for the map function, you need to know that it takes one parameter : **doc**. Inside your map function, your logic will determine if the doc needs to be mapped or not. If yes, you will use the **emit()** function to index it. The emit function takes 2 parameters. 

 1. The key to index
 2. The value to emit

At the end, it will create an array with 3 columns : **id**,**key**,**value**.

*Note:  NEVER BUT NEVER emit the doc as the value. This is totally useless since using the **include_docs** parameter will fetch the documents associated to the id.*


---

__Complex keys__

Now let's say that we want to fetch the people according to different parameters. Let's say that I want to query the users on their name, their gender and their children count. 

In this case, we would have a view like this :

    function(doc) {
        if (doc.type === "people") {
            emit([doc.name,doc.gender,doc.childrenCount]);
        }
    }

*For the example, I didn't validate that the objects had the required parameter since I won't cause me any problem. Therefore, it may vary from your context. You might want to check if they have the parameter birthDate for example.*

So now, as you can see, we still have **one** key but it's a complex one. The trick here is that our key is an array so we can have multiple keys. 

Now, you might be asking yourself, but hey, how do I use this? It's weird! Stay calm, I will show you how! 

When you query multiple keys, it's a good idea to know how works the comparison in CouchDB. For more info, take a look at [this](http://docs.couchdb.org/en/stable/couchapp/views/collation.html). The most important thing to know is that, if you are using ranges and you want to query all the elements on one key, you need to use the `starkey=[null]&endkey=[\ufff0]`. Since null is the lowest value and \ufff0 is the highest character, it will get everything between this.

So, If I want to get all the women named Julia, I would do the following:

`http://localhost:5984/db/_design/people/_view/byNameGenderChildren?starkey=["Julia","Female"]&endkey=["Julia","Female",\ufff0]`

Basically, we take all the rows with the key [Julia,Female] and for the third part, we take anything between the lowest value(null) and the highest(\ufff0) wich means everything.

Next, if I want to fetch all the male with 3 children? Easy as this :
`http://localhost:5984/db/_design/people/_view/byNameGenderchildren?startkey[null,"Male",3]&endkey=[\ufff0,"Male",3]`

