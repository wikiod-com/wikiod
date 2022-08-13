---
title: "Custom Directives"
slug: "custom-directives"
draft: false
images: []
weight: 9571
type: docs
toc: true
---

## Syntax
- `Vue.directive(id, definition);`
- `Vue.directive(id, update); //when you need only the update function.`

## Parameters
| Parameter | Details |
| --------- | ------- |
| `id` | String - The directive id that will be used without the `v-` prefix. (Add the `v-` prefix when using it) |
| `definition` | Object - A definition object can provide several hook functions (all optional): `bind`, `update`, and `unbind`|

## Basics
In addition to the default set of directives shipped in core, Vue.js also allows you to register custom directives. Custom directives provide a mechanism for mapping data changes to arbitrary DOM behavior.

You can register a global custom directive with the `Vue.directive(id, definition)` method, passing in a directive id followed by a definition object. You can also register a local custom directive by including it in a component’s `directives` option.

 **Hook Functions**

 - **bind**: called only once, when the directive is first bound to the element.
 - **update**: called for the first time immediately after `bind` with the initial value, then again whenever the binding value changes. The new value and the previous value are provided as the argument.
 - **unbind**: called only once, when the directive is unbound from the element.

    
    Vue.directive('my-directive', {
         bind: function () {
           // do preparation work
           // e.g. add event listeners or expensive stuff
           // that needs to be run only once
         },
         update: function (newValue, oldValue) {
           // do something based on the updated value
           // this will also be called for the initial value
         },
         unbind: function () {
           // do clean up work
           // e.g. remove event listeners added in bind()
         }    
    })

Once registered, you can use it in Vue.js templates like this (remember to add the `v-` prefix):

    <div v-my-directive="someValue"></div>

When you only need the `update` function, you can pass in a single function instead of the definition object:

    Vue.directive('my-directive', function (value) {
      // this function will be used as update()
    })

**Directive Instance Properties**

All the hook functions will be copied into the actual directive object, which you can access inside these functions as their `this` context. The directive object exposes some useful properties:

 - **el**: the element the directive is bound to.
 - **vm**: the context ViewModel that owns this directive.
 - **expression**: the expression of the binding, excluding arguments and filters.
 - **arg**: the argument, if present.
 - **name**: the name of the directive, without the prefix.
 - **modifiers**: an object containing modifiers, if any.
 - **descriptor**: an object that contains the parsing result of the entire directive.
 - **params**: an object containing param attributes. Explained below.

> You should treat all these properties as read-only and never modify
> them. You can attach custom properties to the directive object too,
> but be careful not to accidentally overwrite existing internal ones.

An example of a custom directive using some of these properties:

HTML

    <div id="demo" v-demo:hello.a.b="msg"></div>

JavaScript

    Vue.directive('demo', {
      bind: function () {
        console.log('demo bound!')
      },
      update: function (value) {
        this.el.innerHTML =
          'name - '       + this.name + '<br>' +
          'expression - ' + this.expression + '<br>' +
          'argument - '   + this.arg + '<br>' +
          'modifiers - '  + JSON.stringify(this.modifiers) + '<br>' +
          'value - '      + value
      }
    })
    var demo = new Vue({
      el: '#demo',
      data: {
        msg: 'hello!'
      }
    })

Result

    name - demo
    expression - msg
    argument - hello
    modifiers - {"b":true,"a":true}
    value - hello!

**Object Literal**

If your directive needs multiple values, you can also pass in a JavaScript object literal. Remember, directives can take any valid JavaScript expression:

HTML

    <div v-demo="{ color: 'white', text: 'hello!' }"></div>

JavaScript 

    Vue.directive('demo', function (value) {
      console.log(value.color) // "white"
      console.log(value.text) // "hello!"
    })

**Literal Modifier**

When a directive is used with the literal modifier, its attribute value will be interpreted as a plain string and passed directly into the `update` method. The `update` method will also be called only once, because a plain string cannot be reactive.

HTML

    <div v-demo.literal="foo bar baz">

JavaScript

    Vue.directive('demo', function (value) {
      console.log(value) // "foo bar baz"
    })

