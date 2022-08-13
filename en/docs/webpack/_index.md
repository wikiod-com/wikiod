---
title : webpack Tutorial
slug : webpack-tutorial
weight : 9850
draft : false
images : []
type : docs
---

Webpack is a module bundler which reads modules with dependencies and produces static assets representing those modules.

It features an extendable [*loader system*][1] which allows bundles to include not only Javascript assets, but CSS, Images, HTML and much more. 

For example, using the in-built Javascript loader, [css-loader][2] and [url-loader][3]:

    require("./code.js") // Load Javascript dependency
    var css = require("./styles.css"); // Load CSS as a string
    var base64Image = require("./image.png"); // Load an image as a base64 string

Would become a single bundled file:

    // From code.js
    console.log("Hello, World");
    // From styles.css
    var css = "body { margin: 0; padding: 0; } h1 { color: #FF0000; }";
    // From image.png
    var base64Image = "data:image/gif;base64,R0lGODlhPQBEAPeoAJosM//AwO/AwHVYZ/z595kzAP/s7P+goOXMv8+fhw/v739/f+8PD98fH/8mJl+fn/9ZWb8/PzWlwv///6wWGbImAPgTEMImIN9gUFCEm/gDALULDN8PAD6atYdCTX9gUNKlj8wZAKUsAOzZz+UMAOsJAP/Z2ccMDA8PD/95eX5NWvsJCOVNQPtfX/8zM8+QePLl38MGBr8JCP+zs9myn/8GBqwpAP/GxgwJCPny78lzYLgjAJ8vAP9fX...";

Dependencies can be defined in any of the most common module styles (CommonJS & AMD).


  [1]: http://webpack.github.io/docs/list-of-loaders.html
  [2]: https://github.com/webpack/css-loader
  [3]: https://github.com/webpack/url-loader

