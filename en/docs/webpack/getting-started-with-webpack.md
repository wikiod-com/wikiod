---
title: "Getting started with webpack"
slug: "getting-started-with-webpack"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Example of Javascript + CSS + Fonts + Images
Required modules

    npm install --save-dev webpack extract-text-webpack-plugin file-loader css-loader style-loader

Folder structure

    .
    └── assets
        ├── css
        ├── images
        └── js

webpack.config.js

<!-- language: lang-js -->

    const webpack = require('webpack');
    const ExtractTextPlugin = require('extract-text-webpack-plugin');
    const path = require('path');
    const glob = require('glob');
    
    module.exports = {
      entry: {
        script: path.resolve(__dirname, './assets/js/app.js'),
        style: path.resolve(__dirname, './assets/css/app.css'),
        images: glob.sync(path.resolve(__dirname, './assets/images/**/*.*')),
      },
      context: __dirname,
      output: {
        path: path.resolve('./dist/assets'),
        publicPath: '/dist/assets',
        filename: '[name].js',
      },
      module: {
        loaders: [
          {
            test: /\.css$/,
            loader: ExtractTextPlugin.extract({
                fallback: 'style-loader',
                use: 'css-loader'
            }),
          },
          {
            test: /(\.woff2?|\.woff|\.ttf|\.eot|\.svg)(\?v=\d+\.\d+\.\d+)?$/,
            loader: 'file-loader?name=[name]-[hash:6].[ext]',
          },
          {
            test: /\.(png|jpe?g|gif|ico)$/,
            loader: 'file-loader?name=[name].[ext]',
          },
        ],
      },
      plugins: [
        new ExtractTextPlugin('app.css' /* optional: , { allChunks: true } */),
      ],
    };

`glob.sync('./assets/images/**/*.*')` will require all files in the images folder as entry.

`ExtractTextPlugin` will grab the generated output and create a bundled `css` file.

## Installation
**Prerequisites:**

[NodeJS][1] and [npm][2]

There are two ways of installing Webpack: globally or per-project. It is best to have the dependency installed per-project, as this will allow you to use different versions of webpack for each project and don't require user to have installed webpack globally.

**Installing per-project**

Run the following command from the root folder of your project:

    npm install webpack --save-dev

You can then run the webpack executable installed to `node_modules`:

    ./node_modules/.bin/webpack

Or create an NPM script in your `package.json` file, where you can omit the `node_modules` part - npm is smart enought to include that folder in its PATH.

<!-- language: lang-js -->

    // in package.json:
    {
      ...
      "scripts": {
        "start": "webpack"
      },
      ...
    }

    // from terminal:
    npm start

**Installing globally**

Run the following command at a prompt:

    npm install webpack -g


  [1]: https://nodejs.org/
  [2]: https://www.npmjs.com/

## Example of webpack.config.js with babel
Dependencies

    npm i -D webpack babel-loader

webpack.config.js

<!-- language: lang-js -->
    const path = require('path');

    module.exports = {
      entry: {
        app: ['babel-polyfill', './src/'],
      },
      output: {
        path: __dirname,
        filename: './dist/[name].js',
      },
      resolve: {
        extensions: ['', '.js'],
      },
      module: {
        loaders: [{
          test: /\.js$/, 
          loaders: ['babel-loader'],
          include: path.resolve(__dirname, 'src')
        }],
      }
    };

## Webpack Simple Example
The minimum required to use Webpack is the following command:

    webpack ./src/index.js ./dist/bundle.js
    
    // this is equivalent to:

    webpack source-file destination-file


Web pack will take the source file, compile to the output destination and resolve any dependencies in the source files.

## Webpack, React JSX, Babel, ES6, simple config
Ensure that you install the correct npm dependencies (babel decided to split itself into a bunch of packages, something to do with "peer dependencies"):

`npm install webpack webpack-node-externals babel-core babel-loader babel-preset-react babel-preset-latest --save`

`webpack.config.js`:

<!-- language: lang-js -->

    module.exports = {
        context: __dirname, // sets the relative dot (optional)
        entry: "./index.jsx",
        output: {
            filename: "./index-transpiled.js"
        },
        module: {
            loaders: [{
                test: /\.jsx$/,
                loader: "babel?presets[]=react,presets[]=latest" // avoid .babelrc
            }]
        }, // may need libraryTarget: umd if exporting as a module
        externals: [require("webpack-node-externals")()], // probably not required
        devtool: "inline-source-map"
    };


`webpack-node-externals` is a function that scans your `node_modules` and ensures that they aren't transpiled and bundled along with your front-end code, though it ensures the bundle retains reference to them. This helps with faster transpilation, since you're not re-encoding libraries.

## Simple webpack setup with Node.js
**Folder Structure**

    .
    ├── lib
    ├── modules
    |   ├── misc.js
    |   ├── someFunctions.js
    ├── app.js
    ├── index.html
    ├── package.json
    ├── webpack.config.js
    └── webserver.js   



**package.json**

    {
      "name": "webpack-example-with-nodejs",
      "version": "1.0.0",
      "description": "Example using webpack code-splitting with some Node.js to support the example",
      "main": "webserver.js",
      "scripts": {
        "test": "echo \"Error: no test specified\" && exit 1"
      },
      "author": "@Gun",
      "license": "ISC",
      "devDependencies": {
        "body-parser": "^1.17.1",
        "express": "^4.15.2",
        "http": "0.0.0",
        "morgan": "^1.8.1",
        "multer": "^1.3.0",
        "webpack": "^2.4.1"
      }
    }


**webpack.config.js**

    var path = require('path'); // used to get context
    
    module.exports = {
        context: path.join(__dirname, 'app'), // resolves entry below, must be absolute path
        entry: './app.js', // entry point or loader for the application
        output: {
            path: path.join(__dirname, 'app/lib'), // express static folder is at /app/lib
            filename: '[name].bundle.js', // the file name of the bundle to create.  [name] is replaced by the name of the chunk (code-splitting)
            publicPath: 'static' // example uses express as the webserver
        }
    };

**webserver.js**

    var express = require('express'),
        path = require('path'),
        bodyParser = require('body-parser'),
        multer = require('multer')()
        logger = require('morgan'),
        fs = require('fs'),
        http = require('http');
    
    var app = express();
    var port = 31416;
    
    app.use(bodyParser.urlencoded({ extended: false }));
    app.use(bodyParser.json());
    app.use(logger('short'));
    app.use('/jsBundles',express.static('lib'));
    app.get('/', function(request, response){
        response.sendFile(__dirname + '/index.html');
    });
    
    var server = http.createServer(app).listen(port, function(){
        console.log("I feel a disturbance in the port:" + port);
    });

**index.html**

    <!DOCTYPE html>
    <html>
        <body>
            <div id="someValue"><label for="num">Enter a number:</label><input id="num" /></div>
            <div class="buttonList">
                <ul>
                    <li><button id="doubleIt">double it</button></li>
                    <li><button id="tripleIt">triple it</button></li>
                </ul>
            </div>
            <div id="someOtherValue">
                And the count shall be: <span id="theCount"></span>
            </div>
            <script src="/jsBundles/main.bundle.js"></script>        
        </body>
    </html>

**app.js**

    require(['./modules/someFunctions'],function(){
            window.onload = function(){
                    var someFunctions  = require('./modules/someFunctions');             
                    document.getElementById('doubleIt').onclick = function(){
                            var num = document.getElementById('num').value;
                            document.getElementById('theCount').innerHTML = someFunctions.Double(num);
                    };
    
                    document.getElementById('tripleIt').onclick = function(){
                            var num = document.getElementById('num').value;
                            document.getElementById('theCount').innerHTML = someFunctions.Triple(num);
                    };
            };
    });

**misc.js**

    var self = {};
    self.isNumber = function(value){
        // http://stackoverflow.com/questions/9716468/is-there-any-function-like-isnumeric-in-javascript-to-validate-numbers
        return !isNaN(parseFloat(value)) && isFinite(value);
    };
    module.exports = self;

**someFunctions.js**

    require(['./misc'], function(){
        var misc= require('./misc');
    
        var self = {};
        self.Double = function(value){
            if(!misc.isNumber(value)){
                return 0;
            };
            return value*2;
        }
    
        self.Triple = function(value){
            if(!misc.isNumber(value)){
                return 0;
            };
            return value*3;
        }
    
        module.exports = self;
    });

**NOTE**

run ***npm i --save-dev*** to install dependencies

run ***node .\node_modules\webpack\bin\webpack.js*** once dependencies are installed

run ***node webserver.js*** to start the server

