---
title: "Setting Up React Environment"
slug: "setting-up-react-environment"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Simple React Component

We want to be able to compile below component and render it in our webpage

__Filename__: src/index.jsx
```javascript

import React from 'react';
import ReactDOM from 'react-dom';

class ToDo extends React.Component {
    render() {
        return (<div>I am working</div>);
    }
}

ReactDOM.render(<ToDo />, document.getElementById('App'));

```

## Install all dependencies
```bash
# install react and react-dom
$ npm i react react-dom --save

# install webpack for bundling
$ npm i webpack -g

# install babel for module loading, bundling and transpiling
$ npm i babel-core babel-loader --save

# install babel presets for react and es6
$ npm i babel-preset-react babel-preset-es2015 --save
```

## Configure webpack
Create a file `webpack.config.js` in the root of your working directory

__Filename__: webpack.config.js
```javascript
module.exports = {
    entry: __dirname + "/src/index.jsx",
    devtool: "source-map",
    output: {
        path: __dirname + "/build",
        filename: "bundle.js"
    },
    module: {
        loaders: [
            {test: /\.jsx?$/, exclude: /node_modules/, loader: "babel-loader"}
        ]
    }
}
```

## Configure babel
Create a file `.babelrc` in the root of our working directory

__Filename__: .babelrc
```
{
    "presets": ["es2015","react"]
}
```

## HTML file to use react component
Setup a simple html file in the root of the project directory

__Filename__: index.html
```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title></title>
  </head>
  <body>
    <div id="App"></div>
    <script src="build/bundle.js" charset="utf-8"></script>
  </body>
</html>
```

## Transpile and bundle your component
Using webpack, you can bundle your component:
```bash
$ webpack
```
This will create our output file in `build` directory.

Open the HTML page in a browser to see component in action

