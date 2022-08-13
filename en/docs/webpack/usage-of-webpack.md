---
title: "Usage of Webpack"
slug: "usage-of-webpack"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Usage CommonJS modules example
Create folder. Open it in command line. Run `npm install webpack -g`. Create 2 files:

cats.js:

    var cats = ['dave', 'henry', 'martha'];
    module.exports = cats;

app.js 

    cats = require('./cats.js');
    console.log(cats);

Run in command line: `webpack ./app.js app.bundle.js`

Now in folder will be file `app.bundle.js`. You can include it in index.html page, open it in browser and see result in console. 

But more fast way is run in command line: `node app.bundle.js` and see result immediately in console: 
> [ 'dave', 'henry', 'martha' ]


## Usage AMD modules example
Create folder. Open it in command line. Run `npm install webpack -g`. Create 2 files:

cats.js:

    define(function(){
        return ['dave', 'henry', 'martha'];
    });

app.js 

    require(['./cats'],function(cats){
        console.log(cats);
    })


Run in command line: 

> webpack ./app.js app.bundle.js

Now in folder will be file: `app.bundle.js`. 

Create index.html file:

    <html>
        <body>
            <script src='app.bundle.js' type="text/javascript"></script>
        </body>
    </html>

Open it in browser and see result in console:
> [ 'dave', 'henry', 'martha' ]


## Usage ES6 (Babel) modules example
as written in [MDN][1] at July 2016: 

> This feature is not implemented in any browsers natively at this time. It is implemented in many transpilers, such as the Traceur Compiler, Babel or Rollup.

So here is example with Babel loader for Webpack:

Create folder. Add package.json file there:

    {
      "devDependencies": {
        "babel-core": "^6.11.4",
        "babel-loader": "^6.2.4",
        "babel-preset-es2015": "^6.9.0",
        "webpack": "^1.13.1"
      }
    }

Open folder in command line. Run:

>  `npm install`.

Create 2 files:

**cats.js**:

    export var cats = ['dave', 'henry', 'martha'];


**app.js**:

    import {cats} from "./cats.js";
    console.log(cats);


For proper using of babel-loader should be added **webpack.config.js** file:

     module: {
      loaders: [
        {
          test: /\.js$/,
          exclude: /(node_modules|bower_components)/,
          loader: 'babel?presets[]=es2015'
        }
      ]
    }

Run in command line: 

> webpack ./app.js app.bundle.js

Now in folder will be file: `app.bundle.js`. 

Create **index.html** file:

    <html>
        <body>
            <script src='app.bundle.js' type="text/javascript"></script>
        </body>
    </html>

Open it in browser and see result in console:
> [ 'dave', 'henry', 'martha' ]


  [1]: https://developer.mozilla.org/en/docs/web/javascript/reference/statements/import/

## Usage ES6 (Typescript) modules example
as written in [MDN][1] at July 2016: 

> This feature is not implemented in any browsers natively at this time. It is implemented in many transpilers, such as the Traceur Compiler, Babel or Rollup.

So here is example with Typescript loader for Webpack:

//TODO

Create simplified version of this article: http://www.jbrantly.com/typescript-and-webpack/ without tsd and jquery.

