---
title: "Models"
slug: "models"
draft: false
images: []
weight: 9992
type: docs
toc: true
---


Sails comes installed with a powerful ORM/ODM called Waterline, a datastore-agnostic tool that dramatically simplifies interaction with one or more databases. It provides an abstraction layer on top of the underlying database, allowing you to easily query and manipulate your data without writing vendor-specific integration code.

## Basic Model
This example shows how to define a simple model in Sails.js

You can generate an empty model file by typing

```
sails generate model car
```

You'll find the new file `Car.js` in `api/models/`.

Next, you fill in some details.

    modules.exports = {
    
      tableName : 'cars',
      connection : 'mongodb',

      attributes : {

        id : { 
          type : 'integer', 
          unique : true, 
          primaryKey : true, 
          autoIncrement : true
        },

        brand : {
          type : 'string',
          size : 25
        },

        description : { 
          type: 'text', 
          defaultsTo : ''
        },
        
        price : {
          type : 'float',
          required : true
        },
                      
        seats : {
          type : 'integer'
        },
            
        sell_date : {
          type : 'datetime'
        },
            
        has_cooler : { 
          type : 'boolean',
          columnName : 'cooler'
        },
                           
        chassis_number : {
          unique : true,
          type : 'string'
        },

        color : {
          type : 'string',
          enum: ['white', 'red', 'black']
        }

      }

    };

 The example above uses nearly every possible model option, which are explained below.


  **1. tableName**

This parameter defines the name of the table that will be created in the database. If not defined, the model name will be used (`car` in this example).

  **2. connection**

This particular defines the database connection used for the model. The details of that connection are defined under the `mongodb` key inside `config/connections.js`. Here's the format of a connection:

    mongodb : {

      // The driver that connect our models with the database
      adapter : '<adapter>',

      // The database parameters
      user : '<username>',
      port : <port>,
      host : '<host>',
      database : '<database>'
    
    }


 **3. attributes**

Each attribute references a column in the database table for the model. In this example, nine columns will be created. 
Each column can be configured with one or more of the following keys:

 - **type** : The data type of the column. [This page]( http://sailsjs.org/documentation/concepts/models-and-orm/attributes#?attribute-options) lists all the available types.
 - **unique** : If true, an error will occur if you try to create an object that has the same value for this column as one already in the database. 
 - **primaryKey** : If true, the column will work as primary key.
 - **autoIncrement** : A sequence will be associated to the column with an auto incrementable number starting at 0.  
 - **size** : The maximum length of the column.
 - **required** : If true, it can't be null.
 - **columnName** : This configures the column in the database, which defaults to the attribute name.
 - **enum** : We can set an array of possible options for an attribute.


