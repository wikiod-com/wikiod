---
title: "Event Bus"
slug: "event-bus"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Event buses are a useful way of communicating between components which are not directly related, i.e. Have no parent-child relationship.

It is just an empty `vue` instance, which can be used to `$emit` events or listen `$on` the said events.

## Syntax
1. export default new Vue()


Use vuex if your application has a lot of components requiring the data of each other.

## eventBus
    // setup an event bus, do it in a separate js file
    var bus = new Vue()

    // imagine a component where you require to pass on a data property
    // or a computed property or a method!

    Vue.component('card', {
      template: `<div class='card'>
        Name: 
          <div class='margin-5'>
              <input v-model='name'>
        </div>
        <div class='margin-5'>
          <button @click='submit'>Save</button>
        </div>
      </div>`,
      data() {
        return {
          name: null
        }
      },
      methods: {
        submit() {
          bus.$emit('name-set', this.name)
        }
      }
    })
    
    // In another component that requires the emitted data.
    var data = {
      message: 'Hello Vue.js!'
    }
    
    var demo = new Vue({
      el: '#demo',
      data: data,
      created() {
        console.log(bus)
        bus.$on('name-set', (name) => {
          this.message = name
        })
      }
    })
    

