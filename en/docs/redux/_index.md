---
title : redux Tutorial
slug : redux-tutorial
weight : 9878
draft : false
images : []
type : docs
---

Redux is a JavaScript library that implements state container of the Flux-based architecture.

Redux can be described in three fundamental principles:

 1. Single source of truth (Single store)
 2. State is read only (need actions to signal change)
 3. Changes are made with pure functions (This creates new state according to the actions)

Main parts:

- store constructor
- store.dispatch(action)
- middleware
- reducers

Redux is astonishingly simple. It uses a function called a reducer (a name derived from the JavaScript reduce method) that takes two parameters: An action, and a next state.

The reducer has access to the current (soon to be previous) state, applies the given action to that state, and returns the desired next state.

Reducers are designed to be pure functions; meaning, they produce no side effects. If you pass the same input values to a reducer 100 times, you will get the exact same output value 100 times. Nothing weird happens. They are completely predictable. As someone with a prominent "NO SURPRISES" sticky note on my monitor, this is a wonderful idea to contemplate.

Reducers do not store state, and they do NOT mutate state. They are passed state, and they return state. This is what reducers look like in action. http://redux.js.org/docs/basics/Reducers.html

Reference: http://redux.js.org/docs/introduction/Motivation.html

