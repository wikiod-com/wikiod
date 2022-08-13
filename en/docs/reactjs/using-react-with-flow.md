---
title: "Using React with Flow"
slug: "using-react-with-flow"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

How to use the [Flow type checker](https://flow.org/) to check types in React components.

[Flow | React](https://flowtype.org/docs/react.html)

## Using Flow to check prop types of stateless functional components

<!-- language-all: lang-js -->

    type Props = {
      posts: Array<Article>,
      dispatch: Function,
      children: ReactElement
    }

    const AppContainer =
      ({ posts, dispatch, children }: Props) => (
        <div className="main-app">
          <Header {...{ posts, dispatch }} />
          {children}
        </div>
      )

## Using Flow to check prop types
<!-- language-all: lang-js -->

    import React, { Component } from 'react';
    
    type Props = {
      posts: Array<Article>,
      dispatch: Function,
      children: ReactElement
    }
    
    class Posts extends Component {
      props: Props;
    
      render () {
        // rest of the code goes here
      }
    }


