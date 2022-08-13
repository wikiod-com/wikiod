---
title: "Getting started with Vue.js"
slug: "getting-started-with-vuejs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## "Hello, World!" Program
To start using [Vue.js](https://vuejs.org), make sure you have the script file included in your HTML. For example, add the following to your HTML.

<!-- language: HTML -->

    <script src="https://npmcdn.com/vue/dist/vue.js"></script>

# Simple Example

## HTML template
<!-- language: HTML -->

    <div id="app">
      {{ message }}
    </div>

## JavaScript

<!-- language: javascript -->

    new Vue({
      el: '#app',
      data: {
        message: 'Hello Vue.js!'
      }
    })

See a [live demo](https://jsfiddle.net/jlam55555/ky667ewg/) of this example.

---

You might also want to check out [the "Hello World" example made by Vue.js](https://jsfiddle.net/yyx990803/okv0rgrk/).

## Hello World in Vue 2 (The JSX way)
JSX is not meant to be interpreted by the browser. It must be first transpiled into standard Javascript. To use JSX you need to install the plugin for babel `babel-plugin-transform-vue-JSX`

Run the Command below:

    npm install babel-plugin-syntax-jsx babel-plugin-transform-vue-jsx babel-helper-vue-jsx-merge-props --save-dev

and add it to your `.babelrc` like this:

    {
      "presets": ["es2015"],
      "plugins": ["transform-vue-jsx"]
    }

Sample code with VUE JSX:

    import Vue from 'vue'  
    import App from './App.vue'
    
    new Vue({  
      el: '#app',
      methods: {
        handleClick () {
          alert('Hello!')
        }
      },
      render (h) {
        return (
          <div>
            <h1 on-click={this.handleClick}>Hello from JSX</h1>
            <p> Hello World </p>
          </div>
        )
      }
    })

By using JSX you can write concise HTML/XML-like structures in the same file as you write JavaScript code.

**Congratulations, You're Done :)**

## Handling User Input
VueJS can be used to easily handle user input as well, and the two way binding using v-model makes it really easy to change data easily.

HTML :

    <script src="https://unpkg.com/vue/dist/vue.js"></script>
    <div id="app">
        {{message}}
    <input v-model="message"> 
    </div>

JS :

    new Vue({
      el: '#app',
      data: {
        message: 'Hello Vue.js!'
      }
    })

It is very easy to do a two-way binding in VueJS using `v-model` directive.

Check out a [live example][1] here.


  [1]: https://jsfiddle.net/sankalpsingha/4yb11nq3/4/

