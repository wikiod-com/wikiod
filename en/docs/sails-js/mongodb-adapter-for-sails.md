---
title: "MongoDB Adapter for Sails"
slug: "mongodb-adapter-for-sails"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Configuration
You can configure the database settings in `config/connections.js`.

Example:

    someMongoDb: {
      adapter: 'sails-mongo',
      host: 'localhost', // defaults to `localhost` if omitted
      port: 27017, // defaults to 27017 if omitted
      user: 'username_here', // or omit if not relevant
      password: 'password_here', // or omit if not relevant
      database: 'database_name_here' // or omit if not relevant
    }


Alternatively, you can specify your Mongo configuration as a URL

    someMongoDb: {
      adapter: 'sails-mongo',
      url: mongodb://username:password@hostname:port/database
    }





## Installation
Install from NPM.

    npm install sails-mongo --save

