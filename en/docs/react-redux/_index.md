---
title : react-redux Tutorial
slug : react-redux-tutorial
weight : 9857
draft : false
images : []
type : docs
---

React Redux is a library which provides React bindings for Redux.

React components aware of the Redux store are called "Containers", "Smart Components" or "Higher Order Component" (HOC). Such components, to use Redux, need to:
* Subscribe to the store to get updates from Redux store
* Dispatch actions

Doing this by hand would imply using `store.subscribe` and `store.dispatch(action)` in React containers.

React Redux simplifies the binding between the Redux store and a React container component by way of the `connect` function, which maps Redux state properties and Action creators to the component's props.

`connect` is a function that creates a higher order component. Connect accepts 3 functions (`mapStateToProps`, `mapDispatchToProps`, `mergeProps`) and returns a container component, that wraps the original component to make turn it into a "connected" component:

    import { connect } from 'react-redux';
    
    const Customers = { ... };
    const mapStateToProps = (state) => { ... }
    const mapDispatchToProps = (dispatch) => { ... }
  
    export default connect(mapStateToProps, mapDispatchToProps)(Customers);

See the examples section for a complete example.

Since all container componenents need to access the Redux store, the recommended way is to use a special `<Provider>` component of React Redux, which passes the store to all the children components (internally using React context).

Official documentation:
http://redux.js.org/docs/basics/UsageWithReact.html

GitHub repo:
https://github.com/reactjs/react-redux



