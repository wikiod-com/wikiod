---
title: "Reducer"
slug: "reducer"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

[Reducers][1] change the application state based on the actions fired. 

The state is immutable, that means reducers should be pure: for the same input, you should **always** get the same output. Because of this, mutability is forbidden in reducers.


  [1]: http://redux.js.org/docs/basics/Reducers.html

## Basic reducer
A basic reducer would look like this:

    // Import the action types to recognise them
    import { ACTION_ERROR, ACTION_ENTITIES_LOADED, ACTION_ENTITY_CREATED } from './actions';


    // Set up a default state
    const initialState = {
        error: undefined,
        entities: []
    };

    // If no state is provided, we take the default state
    export default (state = initilState, action) => {

        // Based on the type of action received, we calculate the new state
        switch(action.type) {

            // Set the error
            case ACTION_ERROR:
                // Note that we create a new object, copy the current state into it,
                // and then we make the relevant changes, so we don't mutate the state
                return Object.assign({}, state, { error: action.error });

            // Unset any error, and load the entitites
            case ACTION_ENTITIES_LOADED:
                return Object.assign({}, state, { 
                    entities: action.entities,
                    error: undefined
                };
            
            // Add only one entity. Again, note how we make a new entities array
            // combining the previous one with the new entity
            // instead of directly modifying it, so we don't mutate the state.
            case ACTION_ENTITY_CREATED:
                return Object.assign({}, state, {
                    entities: [action.entity].concat(state.entities)
                }):

            // If the action is not relevant to this reducer, just return the state
            // as it was before.
            default:
                return state;
        }
    };

## Using Immutable
[Immutable][1] is a great library that provides us with immutable versions of widely used types of collections, such as Lists, Stacks, Maps, and more.

It simplifies the manipulation of the state and makes it easier to make pure calculations and avoid mutation.

Let's see how the Basic reducer can be rewritten using Immutable's Map and List structures:

    import { ACTION_ERROR, ACTION_ENTITIES_LOADED, ACTION_ENTITY_CREATED } from './actions';
    
    // Import Immutable
    import Immutable from 'immutable';
    
    
    // Set up a default state using a Map, a structure very similar to objects
    // Note that states in Redux can be anything, not just objects
    const initialState = Immutable.Map({
        error: undefined,
        entities: Immutable.List()
    });

    export default (state = initialState, action) => {

        switch(action.type) {
    
            case ACTION_ERROR:
                return state.set('error', action.error);
    
            case ACTION_ENTITIES_LOADED:
                return state.merge({
                    entities: Immutable.List(action.entities)
                    error: undefined
                });
            
            case ACTION_ENTITY_CREATED:
                return state.set('entities', state.entities.push(action.entity));

            default:
                return state;
        }
    };

As you may have seen, handling the immutable state gets easier using Immutable.


  [1]: https://facebook.github.io/immutable-js/

## Basic example using ES6 spread
    // Import the action types to recognize them
    import { ACTION_ERROR, ACTION_ENTITIES_LOADED, ACTION_ENTITY_CREATED } from './actions';

    // Set up a default state
    const initialState = {
        error: undefined,
        entities: [],
        loading: true
    };

    // If no state is provided, we take the default state
    export default (state = initialState, action) => {

        // Based on the type of action received, we calculate the new state
        switch(action.type) {
    
            // Set the error
            case ACTION_ERROR:
                // We will create new object with state,
                // which should be produced by error action
                return { 
                    entities: [],
                    loading: false,
                    error: action.error
                };
    
            // Unset any error, and load the entities
            case ACTION_ENTITIES_LOADED:
                return {
                    entities: action.entities,
                    error: undefined,
                    loading: false
                };
            
            // Add only one entity. We will use spread operator (...) to merge
            // objects properties and to create new entity
            case ACTION_ENTITY_CREATED:
                return {
                    ...state,
                    entities: [action.entity].concat(state.entities)
                };
    
            // Every action is processed by each reducer,
            // so we need to return same state if we do not want to mutate it
            default:
                return state;
        }
    };

