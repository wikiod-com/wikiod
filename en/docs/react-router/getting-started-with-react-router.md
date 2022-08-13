---
title: "Getting started with React Router"
slug: "getting-started-with-react-router"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World with React and React Router
Once you've installed `react` and `react-router`, Its time to use both of them together.

The syntax is very simple, you specify the `url` and the `component` you want to render when that url is opened


`<Route path="hello" component={ HelloComponent } />`

This means when the url path is `hello`, Render the component `HelloComponent`



---

**FILENAME:** `app.js`

<!-- language: lang-js -->
    'use strict';
    
    import React from 'react';
    import { render } from 'react-dom';
    import { Router, browserHistory, Link } from 'react-router';
    
    // These are just demo components which render different text.

    let DashboardPage = () => (
      <div>
        <h1>Welcome User</h1>
        <p>This is your dashboard and I am an example of a stateless functional component.</p>
        <Link to="/settings">Goto Settings Page</Link>
      </div>
    )

    let SettingsPage = () => (
      <div>
        <h1>Manage your settings</h1>
        <p>display the settings form fields here...or whatever you want</p>
        <Link to="/">Back to Dashboard Page</Link>
      </div>
    )
    
    let AuthLoginPage = () => (
      <div>
        <h1>Login Now</h1>
        <div>
          <form action="">
            <input type="text" name="email" placeholder="email address" />
            <input type="password" name="password" placeholder="password" />
            <button type="submit">Login</button>
          </form>
        </div>
      </div>
    )

    let AuthLogoutPage = () => (
      <div>
        <h1>You have been successfully logged out.</h1>
        <div style={{ marginTop: 30 }}>
          <Link to="/auth/login">Back to login page</Link>
        </div>
      </div>
    )

    let ArticlePage = ({ params }) => (
      <h3>Article {params.id}</h3>
    )
    
    let PageNotFound = () => (
      <div>
        <h1>The page you're looking for doesn't exist.</h1>
      </div>
    )

    // Here we pass Router to the render function.
    render( (
        <Router history={ browserHistory }>
    
          <Route path="/" component={ DashboardPage } />
          <Route path="settings" component={ SettingsPage } />
    
          <Route path="auth">
            <IndexRoute component={ AuthLoginPage } />
            <Route path="login" component={ AuthLoginPage } />
            <Route path="logout" component={ AuthLogoutPage } />
          </Route>
        
          <Route path="articles/:id" component={ ArticlePage } />

          <Route path="*" component={ PageNotFound } />

        </Router>
      ), document.body );

**Route Parameters** : Router path can be configured to take parameters so that we can read the parameter's value at the component. The path in `<Route  path="articles/:id" component={ ArticlePage } />` have a `/:id`. This `id` variable serves the purpose of path parameter and it can be accessed at the component `ArticlePage` by using `{props.params.id}`. 

If we visit `http://localhost:3000/#/articles/123` then `{props.params.id}` at component `ArticlePage` will be resolved to 123. But visiting url `http://localhost:3000/#/articles`, will not work because there is no id parameter. 

The route parameter can be made **optional** by writing it in between a pair of parenthesis:   
`<Route  path="articles(/:id)" component={ ArticlePage } />`

If you want to use **sub routes**, then you can do

<!-- language: lang-js -->
    <Route path="path" component={ PathComponent }>
      <Route path="subpath" component={ SubPathComponent } />
    </Route>

- when `/path` is accessed, `PathComponent` will be rendered
- when `/path/subpath` is is accessed, `PathComponent` will be rendered and `SubPathComponent` will be passed to it as `props.children`


You can use `path="*"` to catch all the routes that doesn't exist and render `404 page not found` page.


## Getting Started
This *getting started* assumes you are working with [create-react-app][1], or something equivalent using Babel and all the goodies out there.

Also check out the great documentation [right here][2].

First, install react-router-dom:

`npm install react-router-dom` or `yarn add react-router-dom`.

Then, create a component that exists of a basic Navbar with two items and basic pages:

    import React from 'react'
    import { BrowserRouter, Route, Link } from 'react-router-dom'

    const Home = () => (
      <div>
        <p>We are now on the HOME page</p>
      </div>
    )

    const About = () => (
      <div>
        <p>We are now on the ABOUT page</p>
      </div>
    )

    const App = () => (
      <BrowserRouter>
        <div>
          <ul>
            <li><Link to="/">Home</Link></li>
            <li><Link to="/about">About</Link></li>
          </ul>
          <hr/>
          <Route path="/" component={Home}/>
          <Route path="/about" component={About}/>
        </div>
      </BrowserRouter>
    )
    export default App

Let's go step by step through this code:

 - `import React from 'react'`: Make sure you import `React`
 - `import { BrowserRouter as Router, Route, Link } from 'react-router-dom'` split up:
 - `BrowserRouter` is the actual router itself. Make sure to wrap your component within the `BrowserRouter` component.
- `Route` is one particular route that can be navigated to
- `Link` is a component that produces an `<a href="...">` tag, which you can use as a hyperlink.


----------


 - `const Home` is a function that returns the homepage.
 - `const About` is a function that returns the About page.


----------


 - `const App` is the main component:

 - `<BrowserRouter>` is the [JSX][3] component that wraps the components in which you want to use the `<Route>` component.
 - '<div>` is a single element to wrap all JSX inside the `BrowserRouter` in. 
 - `<ul>` is the Navbar. It contains a link to Home and a link to About.
 - `<li><Link to="/">Home</Link></li>` links to the homepage. You can see that, since the link refers to "/", an empty relative path renders the homepage.
 - `<li><Link to="/about">About</Link></li>` links to the About page.
 - `<Route path="/" component={Home}/>` describes which component should be rendered if the relative path is `"/"`.
 - `<Route path="/about" component={About}/>` describes which component should be rendered if the relative path is `"/about"`.

Lot to learn from here, but hopefully this explains the fundamentals, so from here you can continue your learnings.


  [1]: https://www.wikiod.com/reactjs/getting-started-with-react#Create React App
  [2]: https://reacttraining.com/react-router/web/guides/quick-start
  [3]: https://www.wikiod.com/reactjs/jsx

## Installation and Setup
To install React Router, just run the npm command

`npm install --save react-router`

And you're done. This is literally all you have to do to install react router.

**Please Note :** `react-router` is dependent on `react`, So make sure you install `react` as well.

**To set up:**

*using an ES6 transpiler, like babel*

    import { Router, Route, Link } from 'react-router'

*not using an ES6 transpiler*

    var Router = require('react-router').Router
    var Route = require('react-router').Route
    var Link = require('react-router').Link



## Installation using UMD build
A build is also available on [npmcdn](https://npmcdn.com/). You can include the script like this:

    <script src="https://npmcdn.com/react-router/umd/ReactRouter.min.js"></script>

The library will be available globally on `window.ReactRouter`.

