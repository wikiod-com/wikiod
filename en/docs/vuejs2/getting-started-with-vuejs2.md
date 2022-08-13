---
title: "Getting started with vuejs2"
slug: "getting-started-with-vuejs2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World: Getting started with vue-cli
1. Install vue-cli:

       npm install -g vue-cli

2. start a project like:

        vue init <template> <project-name>

    where `<template>`:

      1. webpack - A full-featured Webpack + vue-loader setup with hot reload, linting, testing & css extraction.

      2. webpack-simple - A simple Webpack + vue-loader setup for quick prototyping.

      3. browserify - A full-featured Browserify + vueify setup with hot-reload, linting & unit testing.

      4. browserify-simple - A simple Browserify + vueify setup for quick prototyping.

      5. simple - The simplest possible Vue setup in a single HTML file

    For this example I'll use `webpack`

3. The `vue-cli` will let you go through a series of yes/no questions after which you will have a project ready with scaffolding.

4. `cd` into the project directory, it is the `<project-name>` in `vue init <template> <project-name>` and run `npm install`.

5. After the installation, run `npm run dev`.

Your hello world application is ready!


## "Hello, World!" Program
It's similar to Vue.js version 1, version 2 can be directly embedded in a single html file.    
To include Vue.js2 in html file, make sure you have the script file included in the HTML. For example, use the following  HTML. It should display the Hello message correctly.

    <div id="app">{{message}}</div>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/vue/2.2.0/vue.js"></script>
    <script>
        new Vue({
          el: '#app',
          data: {
            message: 'Hello Vue.js2'
          }
        })
    </script>

See a jsfiddle [demo][1] for this example.


  [1]: https://jsfiddle.net/iiinec/5pest1qa/

