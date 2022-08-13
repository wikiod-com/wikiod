---
title: "Lifecycle Hooks"
slug: "lifecycle-hooks"
draft: false
images: []
weight: 9487
type: docs
toc: true
---

## Common Pitfalls: Accessing DOM from the `ready()` hook
A common usecase for the `ready()` hook is to access the DOM, e.g. to initiate a Javascript plugin, get the dimensions of an element etc.

**The problem**

Due to Vue's asynchronous DOM update mechanism, it's not guaranteed that the DOM has been fully updated when the `ready()` hook is called. This usually results in an error because the element is undefined.


**The Solution**

For this situation, the [`$nextTick()`][1] instance method can help. This method defers the execution of the provided callback function until after the next tick, which means that it is fired when all DOM updates are guaranteed to be finished.

Example:

```js

module.exports {
  ready: function () {
    $('.cool-input').initiateCoolPlugin() //fails, because element is not in DOM yet.
    
    this.$nextTick(function() {
      $('.cool-input').initiateCoolPlugin() // this will work because it will be executed after the DOM update.
    })
  }
}
```

  [1]: http://vuejs.org/api/#vm-nextTick

## Hooks for Vue 1.x
+ `init`
    -
    Called synchronously after the instance has been initialized and prior to any initial data observation.


+ `created`
    -
    Called synchronously after the instance is created. This occurs prior to `$el` setup, but after `data observation`, `computed properties`, `watch/event callbacks`, and `methods` have been setup.


+ `beforeCompile`
    -
    Immediately prior to compilation of the Vue instance.


+ `compiled`
    -
    Immediately after compilation has completed. All `directives` are linked but still prior to `$el` being available.


+ `ready`
    -
    Occurs after compilation *and* `$el` are complete and the instance is injected into the DOM for the first time.


+ `attached`
    -
    Occurs when `$el` is attached to the DOM by a `directive` or instance calls `$appendTo()`.


+ `detached`
    -
    Called when `$el` is removed/detached from the DOM or instance method.


+ `beforeDestroy`
    -
    Immediately before the Vue instance is destroyed, but is still fully functional.


+ `destroyed`
    -
    Called after an instance is destroyed. All `bindings` and `directives` have already been unbound and child instances have also been destroyed.

## Using in an Instance
Since *all* lifecycle hooks in `Vue.js` are just `functions`, you can place *any* of them directly in the instance declaraction.

    //JS
    new Vue({
    
        el: '#example',
    
        data: {
            ...
        },
    
        methods: {
            ...
        },
    
        //LIFECYCLE HOOK HANDLING
        created: function() {
            ...
        },
    
        ready: function() {
            ...
        }
    
    });

