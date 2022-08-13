---
title: "Vuex"
slug: "vuex"
draft: false
images: []
weight: 9879
type: docs
toc: true
---

Vuex is a state management pattern + library for Vue.js applications. It serves as a centralised store for all the components in an application, with rules ensuring that the state can only be mutated in a predictable fashion. It also integrates with Vue's official dev tools extension to provide advanced features such as zero-config time-travel debugging and state snapshot export/import.

## What is Vuex?
Vuex is an official plugin for Vue.js which offers a centralised datastore for use within your application. It is heavily influenced by the Flux application architecture which features a unidirectional data flow leading to simpler application design and reasoning.

Within a Vuex application the datastore holds all **shared application state**. This state is altered by **mutations** which are performed in response to an **action** invoking a mutation event via the **dispatcher**.

An example of the data flow in a Vuex application is outlined in the diagram below.
[![Vuex Application Architecture][1]][1]
Diagram used under the [MIT](http://opensource.org/licenses/MIT) licence, originally from the [Official Vuex GitHub repo](https://github.com/vuejs/vuex).

Individual Vue.js application components can access the store object to retrieve data via **getters**, which are pure functions returning a read-only copy of the desired data.

Components can have **actions** which are functions that perform changes to the component's own copy of the data, then use the **dispatcher** to dispatch a mutation event. This event is then handled by the datastore which updates the state as necessary.

Changes are then automatically reflected throughout the application since all components are reactively bound to the store via their getters.


----------


An [example][2] illustrating the use of vuex in a vue project.

    const state = {
        lastClickTime: null
    }

    const mutations = {
        updateLastClickTime: (state, payload) => {
          state.lastClickTime = payload
      }
    }

    const getters = {
      getLastClickTime: state => {
        return new Date(state.lastClickTime)
      }
    }

    const actions = {
        syncUpdateTime: ({ commit }, payload) => {
        commit("updateLastClickTime", payload)
      },
      asyncUpdateTime: ({ commit }, payload) => {
        setTimeout(() => {
          commit("updateLastClickTime", payload)
        }, Math.random() * 5000)
      }
    }

    const store = new Vuex.Store({
      state,
      getters,
      mutations,
      actions
    })

    const { mapActions, mapGetters } = Vuex;

    // Vue 
    const vm = new Vue({
        el: '#container',
      store,
      computed: {
          ...mapGetters([
            'getLastClickTime'
        ])
      },
      methods: {
          ...mapActions([
            'syncUpdateTime',
          'asyncUpdateTime'
        ]),
        updateTimeSyncTest () {
            this.syncUpdateTime(Date.now())
        },
        updateTimeAsyncTest () {
            this.asyncUpdateTime(Date.now())
        }
      }
    })

And the HTML template for the same:

    <div id="container">
      <p>{{ getLastClickTime || "No time selected yet" }}</p>
      <button @click="updateTimeSyncTest">Sync Action test</button>
      <button @click="updateTimeAsyncTest">Async Action test</button>
    </div>

 1. Here the **state** contains **lastClickTime** property initialized as null. This setting up of default values is important to keep the properties **reactive**. **Properties not mentioned in the state** will be available but the changes made thereafter **would not be accessible** by using getters.

 2. The getter used, provides a computed property which will be updated each time a mutation updates the value of the state property.

 3. **Only mutations** are allowed to change the state and its properties, that said, it does so **synchronously only**.

 4. An Action can be used in case of asynchronous updates, where the API call (here mocked by the randomly timed setTimeout) can be made in the action, and after getting the response a mutation can be committed to, to make the change to the state.

  [1]: http://i.stack.imgur.com/O9e4f.png
  [2]: http://jsfiddle.net/codewingx/ybvyspdo/7/

## Why use Vuex?
When building large applications such as Single Page Apps (SPA's), which typically consist of many reusable components they can quickly become difficult to build and maintain. The sharing of data and state between these components can also quickly break down and become difficult to debug and maintain.

By using a centralised application data store the entire application state can be represented in one place making the application more organised. Through the use of a unidirectional data flow, mutations and by scoping component data access to only the data required it becomes much simpler to reason about the component role and how it should affect the application state.

VueJS components are separate entities and they cannot share data between each other easily. To share data without vuex we need to `emit` event with data and then listen and catch that event with `on`.

component 1

    this.$emit('eventWithDataObject', dataObject)

component 2

    this.$on('eventWithDataObject', function (dataObject) {
        console.log(dataObject)
    })

With vuex installed we can simply access its data from any component without a need of listening to events. 

    this.$store.state.myData

We can also change data synchronously with *mutators*, use asynchronous *actions* and get data with *getter* functions.

Getter functions might work as global computed functions. We can access them from components:
    
    this.$store.getters.myGetter

Actions are global methods. We can dispatch them from components:
 
    this.$store.dispatch('myAction', myDataObject)

And mutations are the only way to change data in vuex.We can commit changes:

    this.$store.commit('myMutation', myDataObject)

Vuex code would look like this

    state: {
      myData: {
        key: 'val'
      }
    },
    getters: {
      myGetter: state => {
         return state.myData.key.length
      }
    },
    actions: {
      myAction ({ commit }, myDataObject) {
        setTimeout(() => {
          commit('myMutation', myDataObject)
        }, 2000)
      }
    },
    mutations: {
      myMutation (state, myDataObject) {
        state.myData = myDataObject
      }
    }



## How to install Vuex?
Most of the time that you'll be using Vuex will be in larger component based applications where you likely be using a module bundler such as Webpack or Browserify in conjunction with Vueify if you're using single files.

In this case the easiest way to get Vuex is from NPM. Run the command below to install Vuex and save it to your application dependencies.

     npm install --save vuex

Ensure that you load link Vuex with your Vue setup by placing the following line after your `require('vue')` statement.

    Vue.use(require('vuex'))

Vuex is also available on CDN; you can grab the latest version from cdnjs [here](https://cdnjs.com/libraries/vuex).


## Auto dismissible notifications

This example will register an vuex module dynamically for storing custom notifications that can automatically dismissed 

*notifications.js*

resolve vuex store and define some constants

    //Vuex store previously configured on other side
    import _store from 'path/to/store';
    
    //Notification default duration in milliseconds
    const defaultDuration = 8000;

    //Valid mutation names
    const NOTIFICATION_ADDED = 'NOTIFICATION_ADDED';
    const NOTIFICATION_DISMISSED = 'NOTIFICATION_DISMISSED';

set our module initial state
    
    const state = {
      Notifications: []
    }
    
set our module getters

    const getters = {
      //All notifications, we are returning only the raw notification objects
      Notifications: state => state.Notifications.map(n => n.Raw)
    }
    
set our module Actions

    const actions = {
      //On actions we receive a context object which exposes the 
      //same set of methods/properties on the store instance
      //{commit} is a shorthand for context.commit, this is an 
      //ES2015 feature called argument destructuring
      Add({ commit }, notification) {
        //Get notification duration or use default duration
        let duration = notification.duration || defaultDuration
    
        //Create a timeout to dismiss notification
        var timeOut = setTimeout(function () {
          //On timeout mutate state to dismiss notification
          commit(NOTIFICATION_DISMISSED, notification);
        }, duration);
    
        //Mutate state to add new notification, we create a new object 
        //for save original raw notification object and timeout reference
        commit(NOTIFICATION_ADDED, {
          Raw: notification,
          TimeOut: timeOut
        })
      },
      //Here we are using context object directly
      Dismiss(context, notification) {
        //Just pass payload
        context.commit(NOTIFICATION_DISMISSED, notification);
      }
    }
    
set our module mutations

    const mutations = {
      //On mutations we receive current state and a payload
      [NOTIFICATION_ADDED](state, notification) {
        state.Notifications.push(notification);
      },
      //remember, current state and payload
      [NOTIFICATION_DISMISSED](state, rawNotification) {
        var i = state.Notifications.map(n => n.Raw).indexOf(rawNotification);
        if (i == -1) {
          return;
        }
    
        clearTimeout(state.Notifications[i].TimeOut);
        state.Notifications.splice(i, 1);
      }
    }
    
Register our module with defined state, getters, actions and mutation

    _store.registerModule('notifications', {
      state,
      getters,
      actions,
      mutations
    });

**Usage**

*componentA.vue*

This components displays all notifications as bootstrap's alerts on top right corner of screen, also allows to manually dismiss each notification.

    <template>
    <transition-group name="notification-list" tag="div" class="top-right">
      <div v-for="alert in alerts" v-bind:key="alert" class="notification alert alert-dismissible" v-bind:class="'alert-'+alert.type">
        <button v-on:click="dismiss(alert)" type="button" class="close" aria-label="Close"><span aria-hidden="true">&times;</span></button>
        <div>
          <div>
            <strong>{{alert.title}}</strong>
          </div>
          <div>
            {{alert.text}}
          </div>
        </div>
      </div>
    </transition-group>
    </template>
    
    <script>
    export default {
      name: 'arc-notifications',
      computed: {
        alerts() {
          //Get all notifications from store
          return this.$store.getters.Notifications;
        }
      },
      methods: {
        //Manually dismiss a notification
        dismiss(alert) {
          this.$store.dispatch('Dismiss', alert);
        }
      }
    }
    </script>
    <style lang="scss" scoped>
    $margin: 15px;
    
    .top-right {
        top: $margin;
        right: $margin;
        left: auto;
        width: 300px;
        //height: 600px;
        position: absolute;
        opacity: 0.95;
        z-index: 100;
        display: flex;
        flex-wrap: wrap;
        //background-color: red;
    }
    .notification {
        transition: all 0.8s;
        display: flex;
        width: 100%;
        position: relative;
        margin-bottom: 10px;
        .close {
            position: absolute;
            right: 10px;
            top: 5px;
        }
    
        > div {
            position: relative;
            display: inline;
        }
    }
    .notification:last-child {
        margin-bottom: 0;
    }
    .notification-list-enter,
    .notification-list-leave-active {
        opacity: 0;
        transform: translateX(-90px);
    }
    .notification-list-leave-active {
        position: absolute;
    }
    </style>

*Snippet for add notification in any other component*

    //payload could be anything, this example content matches with componentA.vue 
    this.$store.dispatch('Add', {
      title = 'Hello',
      text = 'World',
      type = 'info',
      duration = 15000
    });

