---
title: "Heroku node.js Hello World"
slug: "heroku-nodejs-hello-world"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

**login**

`heroku login`

**create app**

`heroku create`  or `heroku create your_name`

**clone the example**
```
git clone https://github.com/zoutepopcorn/herokuworld
cd herokuworld
```
**visit app in your browser**
```
https://your_name.herokuapp.com/
```

**Optional**
test it local:
```
heroku local web
```
check: lolhost:5000


So whats different to a normal node.js app?
package.json
```
"scripts": {
  "start": "node index.js"
},
"engines": {
  "node": "7.6.0"
}
```

index.js
```
process.env.PORT
```

Local port: 5000. Heroku will map it to port 80 on your app url.



## Heroku node.js hello world
**index.js**

    var http = require("http");
    
    http.createServer(function(request, response) {
      response.writeHead(200, {"Content-Type": "text/plain"});
      response.write("Heroku world!");
      response.end();
    }).listen(process.env.PORT);

**package.json**

    {
      "name": "node-example",
      "version": "1.0.0",
      "description": "Hello world Heroku",
      "scripts": {
        "start": "node index.js"
      },
    
      "keywords": [
        "example",
        "heroku"
      ],
      "author": "Johan",
      "license": "MIT",
      "engines": {
        "node": "7.6.0"
      }
    }




