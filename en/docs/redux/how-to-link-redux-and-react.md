---
title: "How to link redux and react"
slug: "how-to-link-redux-and-react"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax

- `<Provider store>`
- `connect([mapStateToProps], [mapDispatchToProps], [mergeProps], [options])`

## Parameters
| Argument | Description |
| ------ | ------ |
| store   | Redux store   |
| mapStateToProps | User provided mapping: `(state, ownProps) => resultProps` |


## Provider
To easily link your Redux store to your React components you can use an additional library: [react-redux](https://github.com/reactjs/react-redux).

First, you need to wrap your app in a `Provider`, which is a component that passes your store to be used by child components:


    import { Provider } from 'react-redux';

    /// ... store = createStore()

    const App = () => (
      <Provider store={store}>
        <MyComponent>
      </Provider>
    )

## Memoizing derived data
In your redux store you hold the raw data. Some times the raw data is all you need, but other times you need to derive new data from the raw data, often by combining parts of the raw data.

A common use case for deriving data is filtering a list of data based on a criteria, where both the list and the criteria may be changed.

The following example implements [mapStateToProps](https://www.wikiod.com/redux/how-to-link-redux-and-react#Map state to properties) and filters a list of strings to keep those who match a search string, producing a new prop `filteredStringList` that can be rendered by a React Component.

<!-- language: lang-js -->

    // Example without memoized selectors
    const mapStateToProps(state) => {
        const {stringList, searchString} = state;

        return {
            filteredStringList: stringList
                .filter(string => string.indexOf(searchString) > -1)
        };
    }

To keep the reducers simple you should only hold the list of data and the filtering criteria in the store, which means you need to derive the data on read time (like we did in the example above).

Deriving data on read time poses two problems:

1. If the same data is derived many times it could compromize performance.
2. If the same data is needed in different components, you may find that you are duplicating the code to derive data.

The solution is to use memoized selectors that are defined only once. The React community suggests using the npm library [reselect](https://www.npmjs.com/package/reselect) to create memoized selectors. In the example below we achieve the same result as in the first example, only with memoized selectors.

<!-- language: lang-js -->

    // Create memoized selector for filteredStringList
    import {createSelector} from 'reselect';

    const getStringList = state => state.stringList;

    const getSearchString = state => state.searchString;

    const getFilteredStringList = createSelector(
        getStringList,
        getSearchString,
        (stringList, searchString) => stringList
            .filter(string => string.indexOf(searchString) > -1)
    );

    // Use the memoized selector in mapStateToProps
    const mapStateToProps(state) => {
        return {
            filteredStringList: getStringList(state)
        };
    }


Note that the two first selectors `getStringList` and `getSearchString` are *not* memoized, because they are so simple that it would provide no performance gain. They still need to be created because we need to pass them as dependencies to `createSelector`, so that it knows when to reuse the memoized result and when to compute a new result.

The memoized selector will use the function passed as the last argument passed to `createSelector` to compute the derived data (in our example the function that returns the filtered string list). Each time the memoized selector is called, if the dependencies are not changed since the last time it was called, (in our example `stringList` and `searchString`), the memoized selector will return the previous result, saving the time it would take recompute it.

You can think of selectors (memoized or not memoized) as getters for the store state, just like action-creators are setters.

You can find more examples on computing derived data in the [Redux documentation's recipes section](http://redux.js.org/docs/recipes/ComputingDerivedData.html#computing-derived-data).

## Map state to properties
After you wrapped your app into provider you can use `connect` function to subscribe your component to store changes and provide mapping between Redux state properties and React components' properties:

    import { connect } from 'react-redux';

    const MyComponent = ({data})  => (
      <div>{data}</div>
    );

    const mapStateToProps = (state, ownProps) => ({
      data: state.myComponentData
    });

    connect(mapStateToProps)(MyComponent);

