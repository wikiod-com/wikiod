---
title: "Events"
slug: "events"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

## Events syntax
To send an event: `vm.$emit('new-message');`

To catch an event: `vm.$on('new-message');`

To send an event to all components **down**: `vm.$broadcast('new-message');`

To send an event to all components **up**: `vm.$dispatch('new-message');`

**Note:** `$broadcast` and `$dispatch` are deprecated in Vue2. ([see Vue2 features](https://github.com/vuejs/vue/issues/2873))

## When should I use events ?
The following picture illustrates how component communication should work. The picture comes from [The Progressive Framework](https://docs.google.com/presentation/d/1WnYsxRMiNEArT3xz7xXHdKeH1C-jT92VxmptghJb5Es/edit#slide=id.g1631db019e_0_16) slides of [Evan You](https://twitter.com/youyuxi) (Developer of VueJS).

[![Component communication][1]][1]

Here is an example of how it works :

[**DEMO**](http://jsfiddle.net/57whmzw4/)

**HTML**

    <script type="x-template" id="message-box">
        <input type="text" v-model="msg" @keyup="$emit('new-message', msg)" />
    </script>
     
    <message-box :msg="message" @new-message="updateMessage"></message-box>
    <div>You typed: {{message}}</div>

**JS**

    var messageBox = {
      template: '#message-box',
      props: ['msg']
    };
     
    new Vue({
      el: 'body',
      data: {
        message: ''
      },
      methods: {
        updateMessage: function(msg) {
          this.message = msg;
        }
      },
      components: {
        'message-box': messageBox
      }
    });

----------

## **The example above can be improved !** ##

The example above shows how the component communication works. But in case of a custom input component, to synchronize the parent variable with the value typed, we sould use `v-model`.

[**DEMO Vue1**](http://jsfiddle.net/2u6oqoh0/)

[**DEMO Vue2**](https://jsfiddle.net/1fe0wasd/)

In Vue1, you should use `.sync` on the prop sent to the `<message-box>` component. This tells VueJS to synchronize the value in the child component with the parent's.

> **Remember:** Every component instance has its own isolated scope.

**HTML Vue1**

    <script type="x-template" id="message-box">
      <input v-model="value" />
    </script>
    
    <div id="app">
      <message-box :value.sync="message"></message-box>
      <div>You typed: {{message}}</div>
    </div>

In Vue2, there is a special `'input'` event you can `$emit`. Using this event allows you to put a `v-model` directly on the `<message-box>` component.
The example will look as follow:

**HTML Vue2**

    <script type="x-template" id="message-box">
      <input :value="value" @input="$emit('input', $event.target.value)" />
    </script>
    
    <div id="app">
      <message-box v-model="message"></message-box>
      <div>You typed: {{message}}</div>
    </div>

**JS Vue 1 & 2**

    var messageBox = {
      template: '#message-box',
      props: ['value']
    };
    
    new Vue({
      el: '#app',
      data: {
        message: ''
      },
      components: {
        'message-box': messageBox
      }
    });

Notice how faster the input is updated.

  [1]: http://i.stack.imgur.com/G1YWd.png

## How to deal with deprecation of $dispatch and $broadcast? (bus event pattern)
You might have realized that `$emit` is scoped to the component that is emitting the event. That's a problem when you want to communicate between components far from one another in the component tree.

**Note:** In Vue1 you coud use `$dispatch` or `$broadcast`, but not in Vue2. The reason being that it doesn't scale well.
There is a popular `bus` pattern to manage this:

[**DEMO**](http://jsfiddle.net/dunyxvm0/)

**HTML**

    <script type="x-template" id="sender">
      <button @click="bus.$emit('new-event')">Click me to send an event !</button>
    </script>
     
    <script type="x-template" id="receiver">
      <div>I received {{numberOfEvents}} event{{numberOfEvents == 1 ? '' : 's'}}</div>
    </script>
     
    <sender></sender>
    <receiver></receiver>

**JS**

    var bus = new Vue();
     
    var senderComponent = {
      template: '#sender',
      data() {
        return {
          bus: bus
        }
      }
    };
     
    var receiverComponent = {
      template: '#receiver',
      data() {
        return {
          numberOfEvents: 0
        }
      },
      ready() {
        var self = this;
     
        bus.$on('new-event', function() {
          ++self.numberOfEvents;
        });
      }
    };
     
    new Vue({
      el: 'body',
      components: {
        'sender': senderComponent,
        'receiver': receiverComponent
      }
    });

You just need to understand that any `Vue()` instance can `$emit` and catch (`$on`) an event. We just declare a global `Vue` instance call `bus` and then any component with this variable can emit and catch events from it. Just make sure the component has access to the `bus` variable.

