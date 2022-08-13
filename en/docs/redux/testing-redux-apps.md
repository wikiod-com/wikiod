---
title: "Testing Redux apps"
slug: "testing-redux-apps"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Redux + Mocha
Redux is very functional, so unit testing is very straightforward.

Action creator:

    export function showSidebar () {
      return {
        type: 'SHOW_SIDEBAR'
      }
    }

Action creators unit test:

    import expect from 'expect'
    import actions from './actions'
    import * as type from './constants'
    
    describe('actions', () => {
      it('should show sidebar', () => {
        const expectedAction = {
          type: type.SHOW_SIDEBAR
        }
        expect(actions.showSidebar()).toEqual(expectedAction)
      })
    })



## Testing a Redux store with Mocha and Chai
    import { expect } from 'chai'; 
    import { createStore } from 'redux';
    
    describe('redux store test demonstration', () => {
      describe('testReducer', () => {
        it('should increment value on TEST_ACTION', () => {
          // define a test reducer with initial state: test: 0
          const testReducer = (state = { test: 0 }, action) => {
            switch (action.type) {
              case 'TEST_ACTION':
                return { test: state.test + 1 };
              default:
                return state;
            }
          });

          // create a redux store from reducer
          const store = createStore(testReducer);

          // establish baseline values (should return initial state)
          expect(store.getState().test).to.equal(0);

          // dispatch TEST_ACTION and expect test to be incremented
          store.dispatch({ type: 'TEST_ACTION' });
          expect(store.getState().test).to.equal(1);

          // dispatch an unknown action and expect state to remain unchanged
          store.dispatch({ type: 'UNKNOWN_ACTION' });
          expect(store.getState().test).to.equal(1);
        });
      });
    });

