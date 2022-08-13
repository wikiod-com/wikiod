---
title: "Asynchronous Data Flow"
slug: "asynchronous-data-flow"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Redux-thunk: basics
While redux itself is entirely synchronous, you can use a middleware such as `redux-thunk` to handle asynchronous actions.

> A "thunk" is another name for a callback. It is a function that is usually 
> passed as an argument to be called at a later time.

To use, apply the middleware to your redux store:

<!-- language: lang-js -->
    import ReduxThunk from 'redux-thunk';

    const store = createStore(
        reducer,
        applyMiddleware(ReduxThunk)
    );

This allows you to pass a thunk to `dispatch` instead of a plain object. The middleware will recognize the thunk and call it. The thunk takes the store's `dispatch` method as a parameter:

<!-- language: lang-js -->
    // an asynchronous action - "thunk"
    // This will wait 1 second, and then dispatch the 'INCREMENT' action
    const delayedIncrement = dispatch => setTimeout(() => {
        dispatch({
            type: 'INCREMENT'
        });
    }, 1000);

    // dispatch the thunk. 
    // note: no () as we're passing the function itself
    store.dispatch(delayedIncrement);

    

## Using redux-thunk with jQuery.ajax
<!-- language: lang-js -->

    const loadUser = userId => dispatch => {
        dispatch({ type: 'USER_LOADING' });
        $.ajax('/users/' + userId, {
            type: 'GET',
            dataType : 'json'
        }).done(response => {
            dispatch({ type: 'USER_LOADED', user: response });
        }).fail((xhr, status, error) => {
            dispatch({ type: 'USER_LOAD_ERROR', status, error });
        });
    };


To use, dispatch like any other action creator:

<!-- language: lang-js -->

    store.dispatch(loadUser(123));

This will result in an initial `USER_LOADING` action being dispatched, which can be used to display a loading indicator (if so desired), and after the response has been received either a `USER_LOADED` action or `USER_LOAD_ERROR` action will be dispatched, depending on the result of the `$.ajax` request.

## Middleware
When you call `store.dispatch(actionObject)` it is handled synchronously.
I.e. reducers would be called and your store listeners would be notified, your react views would be re-rendered on each dispatched action.

Middleware is what enables you to delay dispatching or even dispatch different actions in the middle. I.e. middleware makes your asynchronous actions look synchronous.

    const myAsyncMiddleware = (store) => {
        return (next) => {
            return (action) => {
                if(action.type === "ASYNC_ACTION") {
                  setTimeout(() => {
                    store.dispatch({ type: "ASYNC_ACTION_RESPONSE" });
                  }, 1000);
                } else {
                  return next(action);
                }
            }
        }
    }

    const store = createStore(
      reducer,
      applyMiddleware(myAsyncMiddleware)
    );

## Action creators
Another approach to handling asynchrony in Redux is to use action creators. In Flux, action creators are special functions that construct action objects and dispatch them.

    myActionCreator(dispatch) {
      dispatch({ type: "ASYNC_ACTION_START" });
      setTimeout(() => {
        dispatch({ type: "ASYNC_ACTION_END" });
      }, 1000)
    }

## Use custom middleware + Superagent
This is example has extracted from [this boilerplate](https://github.com/erikras/react-redux-universal-hot-example).

Custom middleware:

    export default function clientMiddleware() {
      return ({dispatch, getState}) => {
        return next => action => {
          if (typeof action === 'function') {
            return action(dispatch, getState);
          }
    
          const { promise, types, ...rest } = action; // eslint-disable-line no-redeclare
          if (!promise) {
            return next(action);
          }
    
          const [REQUEST, SUCCESS, FAILURE] = types;
          next({...rest, type: REQUEST});
    
          const client = new ApiClient();
          const actionPromise = promise(client);
          actionPromise.then(
            (result) => next({...rest, result, type: SUCCESS}),
            (error) => next({...rest, error, type: FAILURE})
          ).catch((error)=> {
            console.error('MIDDLEWARE ERROR:', error);
            next({...rest, error, type: FAILURE});
          });
    
          return actionPromise;
        };
      };
    }

Wrapping `Superagent` library for API call:

    import superagent from 'superagent';
    import config from '../config';
    
    const methods = ['get', 'post', 'put', 'patch', 'del'];
    
    function formatUrl(path) {
      const adjustedPath = path[0] !== '/' ? '/' + path : path;
      return adjustedPath;
    }
    
    export default class ApiClient {
      constructor(req) {
        methods.forEach((method) =>
          this[method] = (path, { params, data } = {}) => new Promise((resolve, reject) => {
            const request = superagent[method](formatUrl(path));
    
            if (params) {
              request.query(params);
            }
    
            if (data) {
              request.send(data);
            }
    
            request.end((err, { body } = {}) => err ? reject(body || err) : resolve(body));
          }));
      }
      empty() {}
    }



## Using redux-thunk with Promises
    import 'whatwg-fetch';

    function checkStatus(response) {
      if (response.status >= 200 && response.status < 300) {
        return response;
      }
      const error = new Error(response.statusText);
      error.response = response;
      throw error;
    }
    
    function parseJSON(response) {
      return response.json();
    }
    
    function getJSON(endpoint, params) {
      return fetch(endpoint, params)
        .then(checkStatus)
        .then(parseJSON);
    }

    export function action() {
      return dispatch => getJSON('/example-endpoint')
        .then((result) => {
          dispatch({
            type: GET_SUCCESS,
            result,
          });
        })
        .catch((error) => {
          dispatch({ type: GET_FAILURE, error });
        });
    }

