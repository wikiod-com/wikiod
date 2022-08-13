---
title: "List Rendering"
slug: "list-rendering"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Basic Usage
A list can be rendered using the `v-for` directive. The syntax requires that you specify the source array to iterate on, and an alias that will be used to reference each item in the iteration. In the following example we use `items` as the source array, and `item` as the alias for each item.

HTML
----

    <div id="app">
      <h1>My List</h1>
      <table>
        <tr v-for="item in items">
          <td>{{item}}</td>
        </tr>
      </table>
    </div>

Script
----

    new Vue({
      el: '#app',
      data: {
        items: ['item 1', 'item 2', 'item 3']
      }
    })

You can view a working demo [here][1].


  [1]: http://codepen.io/theosherman/pen/mExQzp

## Iteration over an object
`v-for` can be used for iterating over an object keys (and values):

*HTML:*

    <div v-for="(value, key) in object">
      {{ key }} : {{ value }}
    </div>

*Script:*

    new Vue({
      el: '#repeat-object',
      data: {
        object: {
          FirstName: 'John',
          LastName: 'Doe',
          Age: 30
        }
      }
    })

## Only render HTML items
In this example will render five `<li>` tags

    <ul id="render-sample">
      <li v-for="n in 5">
        Hello Loop
      </li>
    </ul>

## Pig countdown list
    <ul>
      <li v-for="n in 10">{{11 - n}} pigs are tanning at the beach. One got fried, and
    </ul>

https://jsfiddle.net/gurghet/3jeyka22/

    

