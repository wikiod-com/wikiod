---
title: "NodeJs Routing"
slug: "nodejs-routing"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

How to set up basic Express web server under the node js and Exploring the Express router.

At last, Using Express Router you can use routing facility in you application and it is easy to implement.

## Express Web Server Routing
**Creating Express Web Server**

Express server came handy and it deeps through many user and community. It is getting popular.

Lets create a Express Server. For Package Management and Flexibility for Dependency We will use NPM(Node Package Manager).

 1. Go to the Project directory and create package.json file.
    ***package.json***
    {
  "name": "expressRouter",
  "version": "0.0.1",
  "scripts": {
    "start": "node Server.js"
  },
  "dependencies": {
    "express": "^4.12.3"
   }
 }
 2. Save the file and install the express dependency using following command *npm install*. This will create node_modules in you project directory along with required dependency.
 3. Let's create Express Web Server.
    Go to the Project directory and create server.js file.
***server.js***

    var express = require("express");
    var app = express();
   
   //Creating Router() object
    
   var router = express.Router();
   
   // Provide all routes here, this is for Home page.
   
    router.get("/",function(req,res){
    res.json({"message" : "Hello World"});
   });

   app.use("/api",router);
  
   // Listen to this Port

   app.listen(3000,function(){
    console.log("Live at Port 3000");
   });

    For more detail on setting node server you can see [here][1].

 4. Run the server by typing following command.

    node server.js

    If Server runs successfully, you will se something like this.[![this][1]][1].

 5. Now go to the browser or postman and made a request

    http://localhost:3000/api/
    
    The output will be [![this][2]][2].

That is all, the basic of Express routing. 

Now let's handle the GET,POST etc.

Change yous server.js file like 

    var express = require("express");
    var app = express();
    
    //Creating Router() object
    
    var router = express.Router();
    
    // Router middleware, mentioned it before defining routes.
    
    router.use(function(req,res,next) {
      console.log("/" + req.method);
      next();
    });
    
    // Provide all routes here, this is for Home page.
    
    router.get("/",function(req,res){
      res.json({"message" : "Hello World"});
    });
    

    app.use("/api",router);
    
    
    app.listen(3000,function(){
      console.log("Live at Port 3000");
    });

Now if you restart the server and made the request to 

    http://localhost:3000/api/

You Will see something like [![this][3]][3]

***Accessing Parameter in Routing***

You can access the parameter from url also, Like *http://example.com/api/:name/*. So name parameter can be access. Add the following code into your server.js 

    router.get("/user/:id",function(req,res){
      res.json({"message" : "Hello "+req.params.id});
    });

 
Now restart server and go to [http://localhost:3000/api/user/Adem][4], the output will be like [![this][4]][4].


  [1]: https://i.stack.imgur.com/3ITls.png
  [2]: https://i.stack.imgur.com/9sCpz.png
  [3]: https://i.stack.imgur.com/3ERM7.png
  [4]: https://i.stack.imgur.com/TNuOh.png

