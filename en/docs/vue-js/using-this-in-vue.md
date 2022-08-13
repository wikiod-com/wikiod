---
title: "Using this in Vue"
slug: "using-this-in-vue"
draft: false
images: []
weight: 9755
type: docs
toc: true
---

One of the most common errors we find in Vue code on StackOverflow is the misuse of `this`. The most common mistakes fall generally in two areas, using `this` in callbacks for promises or other asynchronous functions and using arrow functions to define methods, computed properties, etc.

## WRONG! Using an arrow function to define a method that refers to "this"
    new Vue({
      el:"#app",
      data:{
        foo: "bar"
      },
      methods:{
        // This is wrong! Arrow functions capture "this" lexically
        // and "this" will refer to the window.
        doSomething: () => this.foo = "baz"
      }
    })

## RIGHT! Define methods with the typical function syntax
    new Vue({
      el:"#app",
      data:{
        foo: "bar"
      },
      methods:{
        doSomething: function(){
          this.foo = "baz"
        }
      }
    })

Alternatively, if you are using a javascript compiler or a browser that supports Ecmascript 2015

    new Vue({
      el:"#app",
      data:{
        foo: "bar"
      },
      methods:{
        doSomething(){
          this.foo = "baz"
        }
      }
    })

## WRONG! Using "this" in a callback inside a Vue method.

    new Vue({
      el:"#app",
      data:{
        foo: "bar"
      },
      methods:{
        doSomethingAsynchronous(){
          setTimeout(function(){
            // This is wrong! Inside this function,
            // "this" refers to the window object.
            this.foo = "baz";
          }, 1000);
        }
      }
    })


## RIGHT! Use a closure to capture "this"
 You can capture the correct `this` using a [closure][1].

    new Vue({
      el:"#star-wars-people",
      data:{
        people: null
      },
      mounted: function(){
        // Before executing the web service call, save this to a local variable
        var self = this;
        $.getJSON("http://swapi.co/api/people/", function(data){
          // Inside this call back, because of the closure, self will
          // be accessible and refers to the Vue object.
          self.people = data.results;
        })
      }
    })


  [1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Closures

## RIGHT! Use an arrow function.
    new Vue({
      el:"#star-wars-people",
      data:{
        people: null
      },
      mounted: function(){
        $.getJSON("http://swapi.co/api/people/", data => this.people = data.results);
      }
    })

**Caution!** Arrow functions are a syntax introduced in Ecmascript 2015. It is not yet supported but *all* modern browsers, so only use it if you are targetting a browser you *know* supports it, or if you are compiling your javascript down to ES5 syntax using something like [babel][1].


  [1]: https://babeljs.io/

## RIGHT! Use bind.
You can [bind][1] the callback function.

    new Vue({
      el:"#star-wars-people",
      data:{
        people: null
      },
      mounted:function(){
        $.getJSON("http://swapi.co/api/people/", function(data){
          this.people = data.results;
        }.bind(this));
      }
    })

  [1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/bind

## WRONG! Using "this" inside a promise.
    new Vue({
      el:"#star-wars-people",
      data:{
        people: null
      },
      mounted: function(){
        $.getJSON("http://swapi.co/api/people/", function(data){
          // Again, this is wrong! "this", here, refers to the window.
          this.people = data.results;
        })
      }
    })

