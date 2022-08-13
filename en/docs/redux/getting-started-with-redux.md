---
title: "Getting started with redux"
slug: "getting-started-with-redux"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Basic installation:**

You can download redux javascript file, using this [link][1].

Also you can install redux, using [bower][2] :

    bower install https://npmcdn.com/redux@latest/dist/redux.min.js



Next, you need to include redux to your page:

<!-- language: lang-html -->

    <html>
      <head>
        <script type="text/javascript" src="/path/to/redux.min.js"></script>
      </head>
      <body>
        <div id="app"></div>
        <script>
          // Redux is available as `window.Redux` variable.
        </script>
      </body>
    </html>

<hr/>

**Npm installation:**

If you are using [npm][3], to install redux, you need to run:

    npm install redux --save

Next, to use redux, you need to require it (assuming you are using module bundler, like [webpack][4]):

<!-- language: lang-js -->

    var redux = require('redux');
Or if you are using es6 transpiler, like [babel][5]:

<!-- language: lang-js -->

    import redux from 'redux';


  [1]: https://npmcdn.com/redux@latest/dist/redux.min.js
  [2]: http://bower.io
  [3]: https://www.npmjs.com/
  [4]: http://webpack.github.io
  [5]: http://babeljs.io

## Vanilla Redux Example (without React or others)
You can see the running demo by [clicking here](https://jsfiddle.net/inancgumus/e3067seu/).

**HTML:**

<!-- language: lang-html -->

    <p>
      <span>Counter State</span><br />
      (<em>Will increase each minute</em>):
      <p>
        <span id="counter-state" style="font-weight: bolder"></span>
      </p>
    </p>

    <p>
      <button id="increment-action">+ Increase +</button>
      <button id="decrement-action">- Decrease -</button>
    </p>

**REDUX LOGIC:**

<!-- language: lang-js -->

    // ------------------------ reducer helpers ------------------------
    let reducers = {}

    let addReducer = (reducers, actionType, reducer) =>
      reducers[actionType] = (state, action) => {
        if (action.type == actionType) {
          return reducer(state, action)
        }
      }

    let reducer = (state, action) => {
      if (reducers[action.type]) {
        return reducers[action.type](state, action)
      }
      return state
    }


    // ------------------------ redux setup ------------------------

    const { 
      createStore,
      applyMiddleware
      } = Redux


    // apply logging middleware (not necessary)
    // open the console to see the action logger output on each time new action dispatched
    const actionLogger = ({ dispatch, getState }) => next => action => {
      console.log("action logger: action.type=%s state=%d", action.type, getState())
      return next(action)
    }


    // ------------------------ reducers ------------------------
    // those will be creating new states and returning it,
    // depending on the dispatched actions
    addReducer(reducers, 'INCREMENT', (state, action) => ++state)
    addReducer(reducers, 'DECREMENT', (state, action) => --state)


    const DEFAULT_STATE = 0

    const store = createStore(
      reducer, 
      DEFAULT_STATE, 
      applyMiddleware(actionLogger)
    );


    console.log(createStore)


    // ------------------------ rendering ------------------------
    let render = () => document.getElementById('counter-state').innerHTML = store.getState()

    //
    // IMPORTANT BINDING:
    //
    // store will be dispatching the new state to the render method each time the state changes
    //
    store.subscribe(render)

    //
    // render with the state for the first time
    //
    render()



    // ------------------------ redux actions ------------------------

    // continously increment the counter (the state) each second
    setInterval(() => store.dispatch({type: 'INCREMENT'}), 1000)

    // only increment the counter on click to the increase button
    document
      .getElementById('increment-action')
      .addEventListener('click', () => store.dispatch({type: 'INCREMENT'}))

    // only decrement the counter on click to the decrease button
    document
      .getElementById('decrement-action')
      .addEventListener('click', () => store.dispatch({type: 'DECREMENT'}))

