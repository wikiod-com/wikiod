---
title: "Parse Server"
slug: "parse-server"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Running Parse Server Locally (quickest example)

```
$ npm install -g parse-server mongodb-runner
$ mongodb-runner start
$ parse-server --appId APPLICATION_ID --masterKey MASTER_KEY --databaseURI mongodb://localhost/test
```

You can use any arbitrary string as your application id and master key. These will be used by your clients to authenticate with the Parse Server.

That's it! You are now running a standalone version of Parse Server on your machine, with a locally-hosted Mongo DB as well.

## Saving your first object
Now that you're running Parse Server, it is time to save your first object. We'll use the [REST API](https://parse.com/docs/rest/guide), but you can easily do the same using any of the [Parse SDKs](https://parseplatform.github.io/#sdks). Run the following:

```bash
curl -X POST \
-H "X-Parse-Application-Id: APPLICATION_ID" \
-H "Content-Type: application/json" \
-d '{"score":1337,"playerName":"Sean Plott","cheatMode":false}' \
http://localhost:1337/parse/classes/GameScore
```

You should get a response similar to this:

```js
{
  "objectId": "2ntvSpRGIK",
  "createdAt": "2016-03-11T23:51:48.050Z"
}
```

## Parse Server + Express
You can also create an instance of Parse Server, and mount it on a new or existing Express website:

```js
var express = require('express');
var ParseServer = require('parse-server').ParseServer;
var app = express();

var api = new ParseServer({
  databaseURI: 'mongodb://localhost:27017/dev', // Connection string for your MongoDB database
  cloud: '/home/myApp/cloud/main.js', // Absolute path to your Cloud Code
  appId: 'myAppId',
  masterKey: 'myMasterKey', // Keep this key secret!
  fileKey: 'optionalFileKey',
  serverURL: 'http://localhost:1337/parse' // Don't forget to change to https if needed
});

// Serve the Parse API on the /parse URL prefix
app.use('/parse', api);

app.listen(1337, function() {
  console.log('parse-server-example running on port 1337.');
});
```


