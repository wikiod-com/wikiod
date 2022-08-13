---
title: "Working with JavascriptServices"
slug: "working-with-javascriptservices"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

According to official documentation:

`JavaScriptServices` is a set of technologies for ASP.NET Core developers. It provides infrastructure that you'll find useful if you use Angular 2 / React / Knockout / etc. on the client, or if you build your client-side resources using Webpack, or otherwise want to execute JavaScript on the server at runtime.

## Enabling webpack-dev-middleware for asp.net-core project
Let's say you use `Webpack` for front end bundling. You can add `webpack-dev-middleware` to serve your statics through tiny and fast server. It allows you to automatically reload your assets when content has changed, serve statics in memory without continuously writing intermediate versions on disk.

Prerequisites
=======
## NuGet ##
Install-Package Microsoft.AspNetCore.SpaServices
## npm ##
npm install --save-dev aspnet-webpack, webpack-dev-middleware, webpack-dev-server

Configuring
=======
Extend `Configure` method in your `Startup` class

    if (env.IsDevelopment())
    {
         app.UseWebpackDevMiddleware(new WebpackDevMiddlewareOptions()
         {
             ConfigFile = "webpack.config.js" //this is defualt value
         });
    }

## Add Hot Module Replacement (HMR)
Hot Module Replacement allows to add, change or delete app module when application is running. Page reloading is not needed in this case.

# Prerequisites #
In addition to `webpack-dev-middleware` packages:

`npm install --save-dev webpack-hot-middleware`

# Configuration #

Simply update configuration of `UseWebpackDevMiddleware` with new options:

    app.UseWebpackDevMiddleware(new WebpackDevMiddlewareOptions()
    {
        ConfigFile = "webpack.config.js", //this is defualt value
        HotModuleReplacement = true,
        ReactHotModuleReplacement = true, //for React only
    });

You also need to accept hot modules in your app code.

 HMR is supported for Angular 2, React, Knockout and Vue.

## Generating sample single page application with asp.net core
You can use `aspnetcore-spa` generator for `Yeoman` to create brand-new single page application with asp.net core. 

This allows you to choose one of the popular front end frameworks and generates project with webpack, dev server, hot module replacement and server-side rendering features.


Just run

    npm install -g yo generator-aspnetcore-spa
    cd newproject
    yo aspnetcore-spa

and choose your favorite framework

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/hNv2A.png

