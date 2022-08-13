---
title: "Environment Detection"
slug: "environment-detection"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Advanced Environment Configurations
For more complex applications, you'll want to build up a ``settings.json` object using multiple environment variables.

```
if(Meteor.isServer){
  Meteor.startup(function()){
    // this needs to be run on the server
    var environment, settings;

    environment = process.env.METEOR_ENV || "development";

    settings = {
      development: {
        public: {
          package: {
            name: "jquery-datatables",
            description: "Sort, page, and filter millions of records. Reactively.",
            owner: "LumaPictures",
            repo: "meteor-jquery-datatables"
          }
        },
        private: {}
      },
      staging: {
        public: {},
        private: {}
      },
      production: {
        public: {},
        private: {}
      }
    };

    if (!process.env.METEOR_SETTINGS) {
      console.log("No METEOR_SETTINGS passed in, using locally defined settings.");
      if (environment === "production") {
        Meteor.settings = settings.production;
      } else if (environment === "staging") {
        Meteor.settings = settings.staging;
      } else {
        Meteor.settings = settings.development;
      }
      console.log("Using [ " + environment + " ] Meteor.settings");
    }
  });
}
```

## Specifying app parameters with METEOR_SETTINGS
The METEOR_SETTINGS environment variable can accept JSON objects, and will expose that object in the ``Meteor.settings`` object. First, add a ``settings.json`` to your app root with some configuration info.

<!-- language: lang-js -->
```
{
  "public":{
    "ga":{
      "account":"UA-XXXXXXX-1"
    }
  }
}
```
Then you'll need to launch your application using your settings file.

<!-- language: lang-bash -->
```
# run your app in local development mode with a settings file
meteor --settings settings.json

# or bundle and prepare it as if you're running in production
# and specify a settings file
meteor bundle --directory /path/to/output
cd /path/to/output
MONGO_URL="mongodb://127.0.0.1:27017" PORT=3000 METEOR_SETTINGS=$(cat /path/to/settings.json) node main.js
```

These settings can then be accessed from Meteor.settings and used in your app.

<!-- language: lang-js -->
```
Meteor.startup(function(){
  if(Meteor.isClient){
    console.log('Google Analytics Account', Meteor.settings.public.ga.account);
  }
});
```

## Environment Detection on the Server
Environment variables are also available to the server via the ``process.env`` object.

```js
if (Meteor.isServer) {
  Meteor.startup(function () {
    // detect environment by getting the root url of the application
    console.log(JSON.stringify(process.env.ROOT_URL));

    // or by getting the port
    console.log(JSON.stringify(process.env.PORT));

    // alternatively, we can inspect the entire process environment
    console.log(JSON.stringify(process.env));
  });
}
```

## Client Environment Detection using Meteor Methods
To detect the environment on the server, we have to create a helper method on the server, as the server will determine which environment it is in, and then call the helper method from the client. Basically, we just relay the environment info from the server to the client.

```
//------------------------------------------------------------------------------------------------------
// server/server.js
// we set up a getEnvironment method

Meteor.methods({
  getEnvironment: function(){
    if(process.env.ROOT_URL == "http://localhost:3000"){
        return "development";
    }else{
        return "staging";
    }
  }
 });    

//------------------------------------------------------------------------------------------------------
// client/main.js
// and then call it from the client

Meteor.call("getEnvironment", function (result) {
  console.log("Your application is running in the " + result + "environment.");
});
```

## Client Environment Detection using NODE_ENV
As of Meteor 1.3, Meteor now exposes the ``NODE_ENV`` variable on the client by default.

```js
if (Meteor.isClient) {
  Meteor.startup(function () {
    if(process.env.NODE_ENV === "testing"){
      console.log("In testing...");
    }
    if(process.env.NODE_ENV === "production"){
      console.log("In production...");
    }
  });
}
```

