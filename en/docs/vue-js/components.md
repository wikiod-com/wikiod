---
title: "Components"
slug: "components"
draft: false
images: []
weight: 9919
type: docs
toc: true
---

In Component(s):
  
**props** is an array of string literals or object references used to pass data from parent component.  It can also be in object form when it is desired to have more fine grained control like specifying default values, type of data accepted, whether it is required or optional  

**data** has to be a function which returns an object instead of a plain object. It is so because we require each instance of the component to have its own data for re-usability purpose.  
  
**events** is an object containing listeners for events to which the component can respond by behavioral change  

**methods** object containing functions defining the behavior associated with the component  

**computed** properties are just like watchers or observables, whenever any dependency changes the properties are recalculated automatically and changes are reflected in DOM instantly if DOM uses any computed properties  

**ready** is a Vue instance's life-cycle hook

## Component scoped (not global)
[***Demo***][1]

**HTML**
----

    <script type="x-template" id="form-template">
        <label>{{inputLabel}} :</label>
      <input type="text" v-model="name" />
    </script>
    
    <div id="app">
      <h2>{{appName}}</h2>
      <form-component title="This is a form" v-bind:name="userName"></form-component>
    </div>


**JS**
--

    // Describe the form component
    // Note: props is an array of attribute your component can take in entry.
    // Note: With props you can pass static data('title') or dynamic data('userName').
    // Note: When modifying 'name' property, you won't modify the parent variable, it is only descendent.
    // Note: On a component, 'data' has to be a function that returns the data.
    var formComponent = {
      template: '#form-template',
      props: ['title', 'name'],
      data: function() {
        return {
          inputLabel: 'Name'
        }
      }
    };
    
    // This vue has a private component, it is only available on its scope.
    // Note: It would work the same if the vue was a component.
    // Note: You can build a tree of components, but you have to start the root with a 'new Vue()'.
    var vue = new Vue({
      el: '#app',
      data: {
        appName: 'Component Demo',
        userName: 'John Doe'
      },
      components: {
        'form-component': formComponent
      }
    });


  [1]: https://jsfiddle.net/56t3z02e/

## What are components and how to define components?
Components in Vue are like widgets. They allow us to write reusable custom elements with desired behavior.  

They are nothing but objects which can contain any/all of the options that the root or any Vue instance can contain, including an HTML template to render.  
  
Components consist of:
  - HTML markup: the component's template
  - CSS styles: how the HTML markup will be displayed
  - JavaScript code: the data and behavior

These can each be written in a separate file, or as a single file with the `.vue` extension.
Below are examples showing both ways: 
  
**.VUE** - as a single file for the component  
 
    
    <style>  
        .hello-world-compoment{  
            color:#eeeeee;  
            background-color:#555555; 
        }  
    </style>  
    
    <template>
        <div class="hello-world-component">
            <p>{{message}}</p>
            <input @keyup.enter="changeName($event)"/>
        </div>
    </template>
    
    <script>
        export default{            
            props:[ /* to pass any data from the parent here... */ ],   
            events:{ /* event listeners go here */},
            ready(){
                this.name= "John";
            },
            data(){
               return{
                  name:''
               }
            },
            computed:{
                message(){
                    return "Hello from " + this.name;
                }
            },
            methods:{
                // this could be easily achieved by using v-model on the <input> field, but just to show a method doing it this way.
                changeName(e){
                    this.name = e.target.value;
                }
            }  
        }
    </script>  
  
**Separate Files**  
  
  ***hello-world.js*** - the JS file for the component object  
    
    export default{
        template:require('./hello-world.template.html'),
        props:[ /* to pass any data from the parent here... */ ],  
        events:{ /* event listeners go here */ },
        ready(){
            this.name="John";
        },
        data(){
            return{
                name:''
            }
        },
        computed:{
            message(){
                return "Hello World! from " + this.name;
            }
        },
        methods:{
            changeName(e){
                let name = e.target.value;
                this.name = name;
            }
        }
    }  
  
***hello-world.template.html***  
  
    <div class="hello-world-component">
        <p>{{message}}</p>
        <input class="form-control input-sm" @keyup.enter="changeName($event)">
    </div>  
  
***hello-world.css***  
  
     .hello-world-compoment{  
        color:#eeeeee;  
        background-color:#555555; 
    }    
These examples use es2015 syntax, so Babel will be needed to compile them to es5 for older browsers.  
**Babel** along with **Browserify + vueify** or **Webpack + vue-loader** will be required to compile `hello-world.vue`.  
  
Now that we have the `hello-world` component defined, we should register it with Vue.  
  
This can be done in two ways:  
  
**Register as a global component**  
In the `main.js` file (entry point to the app) we can register any component globally with `Vue.component`:
  
    import Vue from 'vue'; // Note that 'vue' in this case is a Node module installed with 'npm install Vue'
    Vue.component('hello-world', require('./hello-world');  // global registeration
  
    new Vue({
        el:'body',

        // Templates can be defined as inline strings, like so:
        template:'<div class="app-container"><hello-world></hello-world></div>'
    });  
  
**Or register it locally within a parent component or root component**  
  
    import Vue from 'vue'; // Note that 'vue' in this case is a Node module installed with 'npm install Vue'
    import HelloWorld from './hello-world.js';  

    new Vue({
        el:'body',
        template:'<div class="app-container"><hello-world></hello-world></div>",
  
        components:{HelloWorld}  // local registeration
    });  
  
  
***Global Components*** can be used anywhere within the Vue application.
  
***Local Components*** are only available for use in the parent component with which they are registered.


**Fragment component**<br>
You may get a console error telling you that you can't do something because yours is a _fragment component_. To solve this sort of issue just wrap your component template inside a single tag, like a `<div>`.



## Local registration of components
A component can be registered either globally or locally (bind to another specific component).

    var Child = Vue.extend({
        // ...
    })

    var Parent = Vue.extend({
        template: '...',
        components: {
             'my-component': Child
        }
    })

Thiw new component (<my-component>) will only be available inside the scope (template) of the Parent component.

## Inline registration
You can extend and register a component in one step:

    Vue.component('custom-component', {
        template: '<div>A custom component!</div>'
    })

Also when the component is registered locally:

    var Parent = Vue.extend({
        components: {
            'custom-component': {
                template: '<div>A custom component!</div>'
            }
        }
    })

## Data registration in components
Passing an object to the `data` property when registering a component would cause all instances of the component to point to the same data. To solve this, we need to return `data` from a function.

    var CustomComponent = Vue.extend({
        data: function () {
            return { a: 1 }
        }
    })

## Events
One of the ways components can communicate with its ancestors/descendants is via custom communication events. All Vue instances are also emitters and implement a custom event interface that facilitates communication within a component tree. We can use the following:

 - `$on`: Listen to events emitted by this components ancestors or descendants.
 - `$broadcast`: Emits an event that propagates downwards to all descendants.
 - `$dispatch`: Emits an event that triggers first on the component itself and than propagates upwards to all ancestors.
 - `$emit`: Triggers an event on self.

For example, we want to hide a specific button component inside a form component when the form submits. On the parent element:

    var FormComponent = Vue.extend({
      // ...
      components: {
        ButtonComponent
      },
      methods: {
        onSubmit () {
            this.$broadcast('submit-form')
        }
      }
    })

On the child element:

    var FormComponent = Vue.extend({
      // ...
      events: {
        'submit-form': function () {
            console.log('I should be hiding');
        }
      }
    })

Some things to keep in mind:

 - Whenever an event finds a component that is listening to it and gets triggered, it will stop propagating unless the function callback in this component returns `true`.
 - `$dispatch()` always triggers first on the component that has emitted it.
 - We can pass any number of arguments to the events handler. Doing `this.$broadcast('submit-form', this.formData, this.formStatus)` allows us to access this arguments like `'submit-form': function (formData, formStatus) {}`

