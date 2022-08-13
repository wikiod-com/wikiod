---
title: "Getting started with express"
slug: "getting-started-with-express"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World App, using ExpressJS 4 and Node >= 4
# Preface

You'll need `node >= 4` and `express 4` for this project. You can get the latest `node` distribution from [their download page](https://nodejs.org/en/download/).  

Before this tutorial, you should initialize your node project by running

    $ npm init

from the command line and filling in the information you want. Note that you can change the info at any time by editing the `package.json` file.

# Installation

Install `express` with `npm`:

    $ npm install --save express

After installing Express as a node module, we can create our entry point. This should be in the same directory as our `package.json`

    $ touch app.js

# Directory Contents

The folder should have the following directory structure:

    <project_root>
     |-> app.js
     |-> node_modules/
     '-> package.json

# Code

Open `app.js` in your preferred editor and follow these four steps to create your first Express app:

<!-- language: lang-js -->

    // 1. Import the express library.
    import express from 'express';

    // 2. Create an Express instance.
    const app = express();
    
    // 3. Map a route. Let's map it to "/", so we can visit "[server]/".
    app.get('/', function(req, res) {
       res.send('Hello World');
    });
    
    // 4. Listen on port 8080
    app.listen(8080, function() {
       console.log('Server is running on port 8080...');
    });

# Execution

From the project directory, we can run our server using the command 

    $ node app.js

You should see the text
    
    $ Our Express App Server is listening on 8080...

Now, visit `http://localhost:8080/` and you'll see the text "Hello World!"

Congratulations, you've created your first Express app!

## Installation
**Express JS** is the goto framework for developing `Web Applications`, `APIs` and almost any kind of `Backend` using Node.

To install `express`, all you have to do is run the **npm** command

**`npm install express --save`**

And you're done.

---

## To create and run a new express server ##

create a file `app.js` and add this code

    // require express
    var express = require('express');
    var app = express();
    
    // when "/" is opened in url, this function will be called.
    app.get('/', function (req, res) {
      res.json({ code: 200, message: 'success' });
    })
    
    app.listen( 3000, function () {
      console.log('Express server running at http://localhost:3000');
    });


- In your terminal, run `node app.js` and
- Open the url `http://localhost:3000` in web browser to see your newly created express server.


---

It's also a good idea to install `body-parser` and `express-session` along with `express` as most of the time you will want to read the data sent in `POST` request and manage user sessions.

- [body-parser on github][1]
- [express-session on github ][2]


  [1]: https://github.com/expressjs/body-parser
  [2]: https://github.com/expressjs/session

## Starting an application with the Express generator
To get started quickly with Express, you can use the [Express generator](http://expressjs.com/en/starter/generator.html) which will create an application skeleton for you.

First, install it globally with npm:

    npm install express-generator -g

You may need to put `sudo` before this command if you get a "permission denied" error.

Once the generator is installed, you can start a new project like this:

    express my_app

The above command will create a folder called `my_app` with a `package.json` file, an `app.js` file, and a few subfolders like `bin`, `public`, `routes`, `views`.

Now navigate to the folder and install the dependencies:

    cd first_app
    npm install

If you're on Linux or macOS, you can start the app like this:

<!--language: lang-bash-->
    DEBUG=myapp:* npm start

Or, if you're on Windows:

<!--language: lang-bash-->
    set DEBUG=myapp:* & npm start

Now, load http://localhost:3000/ in your web browser and you should see the words "Welcome to Express".


  [1]: http://localhost:3000/

## Creating an EJS app
```
a@coolbox:~/workspace$ express --ejs my-app
a@coolbox:~/workspace$ cd my-app
a@coolbox:~/workspace/my-app$ npm install
a@coolbox:~/workspace/my-app$ npm start
```

