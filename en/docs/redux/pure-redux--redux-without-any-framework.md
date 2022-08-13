---
title: "Pure Redux - Redux without any framework"
slug: "pure-redux---redux-without-any-framework"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Parameters
| Paramenter | Description |
| ------ | ------ |
| action   | It must be an object with at least the `type` property. Any other property can be passed and will be accessible within the reducer function. |

If you're not using bundlers like Webpack and Browserify, change the first line to:

    const { createStore } = Redux;

Or just call it directly from the Redux global when creating the store:

    const store = Redux.createStore(counter);

## Full example
## `index.html`

    <button id="increment">Increment</button>
    <button id="decrement">Decrement</button>
    <p id="app"></p>

- - -

## `index.js`

    import { createStore } from 'redux';

    function counter(state = 0, action) {
      switch (action.type) {
        case 'INCREMENT':
          return state + 1;

        case 'DECREMENT':
          return state - 1;
        
        default:
          return state;
      }
    }

    const store = createStore(counter);

    function render() {
      const state = store.getState();
      document.querySelector('#app').innerHTML = `Counter: ${state}`;
    }

    const incrementButton = document.querySelector('#increment');
    const decrementButton = document.querySelector('#decrement');

    incrementButton.addEventListener('click', () => {
      store.dispatch({ type: 'INCREMENT' });
    });

    decrementButton.addEventListener('click', () => {
      store.dispatch({ type: 'DECREMENT' });
    });

    store.subscribe(() => {
      render();
    });

    render();

