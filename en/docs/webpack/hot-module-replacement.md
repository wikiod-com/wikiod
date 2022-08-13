---
title: "Hot Module Replacement"
slug: "hot-module-replacement"
draft: false
images: []
weight: 9926
type: docs
toc: true
---

webpack-hot-middleware
======================
Use with `webpack-dev-middleware`, by adding `webpack-hot-middleware/client` to entry.

Config
------

Add configs as query string to the path. Example:

    webpack-hot-middleware/client?path=/__what&timeout=2000&overlay=false


Option | Description
------ | -----------
path | The path which the middleware is serving the event stream on
timeout | The time to wait after a disconnection before attempting to reconnect
overlay | Set to false to disable the DOM-based client-side overlay.
reload | Set to true to auto-reload the page when webpack gets stuck.
noInfo | Set to true to disable informational console logging.
quiet | Set to true to disable all console logging.
dynamicPublicPath | Set to true to use webpack publicPath as prefix of path. (We can set `__webpack_public_path__` dynamically at runtime in the entry point, see note of `output.publicPath`)

## Enable HMR for Module
To make a module eligible for Hot Module Replacement (HMR), the simplest way is to add `module.hot.accept()` inside the module, like this:

    // ...
    
    if(module.hot) {
        module.hot.accept(); // This will make current module replaceable
    }

## Use with webpack-dev-middleware
1. Install *webpack-dev-middleware* via npm

       npm i -D webpack-dev-middleware webpack-hot-middleware

2. Modify *webpack.config.js*

   - Add `webpack-hot-middleware/client` to each items defined in `"entry"`
   - Add `new webpack.HotModuleReplacementPlugin()` to `"plugins"`

         module.exports = {
             entry: {
                 js: [
                     './index.js',
                     'webpack-hot-middleware/client?path=/__webpack_hmr&timeout=20000&reload=true'
                 ]
             },
             plugins: [
                 new webpack.HotModuleReplacementPlugin()
             ]
         };

3. Add these to *index.js*

       var webpack = require('webpack');
       var webpackDevMiddleware = require('webpack-dev-middleware');
       var webpackHotMiddleware = require('webpack-hot-middleware');

       var config = require('./webpack.config.js');
       var compiler = webpack(config);

       app.use(webpackDevMiddleware(compiler, {
           noInfo: true,
           publicPath: config.output.publicPath,
           stats: { colors: true },
           watchOptions: {
               aggregateTimeout: 300,
               poll: true
           },
       }));

       app.use(webpackHotMiddleware(compiler, {
           log: console.log,
       }));

## Use with webpack-dev-server
1. Install *webpack-dev-server* via npm.

       npm i -D webpack-dev-server

2. Configure `webpack-dev-server` by adding *server.js*.

       // server.js

       var webpack = require("webpack");
       var WebpackDevServer = require("webpack-dev-server");
       var config = require("./webpack.dev.config");

       var server = new WebpackDevServer(webpack(config), {
           // ...
       });

       server.listen(8080);

2. Modify *webpack.config.js*

   - Add `webpack-dev-server/client` to each items defined in `"entry"`.
   - Add `webpack/hot/only-dev-server` to each items defined in `"entry"`.
     * **NOTE:** Change if needed...
     * Use `webpack/hot/only-dev-server` to block page refresh if HMR fails.
     * Use `webpack/hot/dev-server` to auto-refresh page if HMR fails.
   - Add `new webpack.HotModuleReplacementPlugin()` to `"plugins"`

         module.exports = {
             entry: {
                 js: [
                     'webpack-dev-server/client?http://localhost:8080'
                     'webpack/hot/only-dev-server',
                     './index.js'
                 ]
             },
             plugins: [
                 new webpack.HotModuleReplacementPlugin()
             ]
         };

3. Add `hot: true` in `webpack-dev-server` configuration

       var server = new WebpackDevServer(webpack(config), {
           hot: true

           // ... other configs
       });

