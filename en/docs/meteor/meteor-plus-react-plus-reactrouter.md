---
title: "Meteor + React + ReactRouter"
slug: "meteor-+-react-+-reactrouter"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

This document will show how to use ReactRouter with Meteor and React. From zero to a working app, including roles and authentication.

I'll show each step with an example

1- Create the project

2- Add React + ReactRouter

3- Add Accounts

4- Add Roles packages


## Create the project
1- First all, install https://www.meteor.com/install

2- Create a project. (`--bare` is to create an empty project)

`meteor create --bare MyAwesomeProject` 

3- Create the minimal file structure (`-p` to create intermediate directories):

`cd MyAwesomeProject`
 
`mkdir -p client server imports/api imports/ui/{components,layouts,pages} imports/startup/{client,server}`

4- Now, create an HTML file in client/main.html
``` 
<head>
   <meta charset="utf-8">
   <title>My Awesome Meteor_React_ReactRouter_Roles App</title>
</head>
 
<body>
  Welcome to my Meteor_React_ReactRouter_Roles app
</body>
```

5- Make sure it's working: (3000 is the default port, so you can actually skip the '-p 3000')

` meteor run -p 3000`

and opening your browser on 'localhost:3000'

# Note:
- I'm skipping some other files that you will need to create, to make things shorter. Specifically, you will need to create some index.js files in *client* ,  *imports/startup/{client,server}* and *server* directories.

- You can view a full example in https://github.com/rafa-lft/Meteor_React_Base. Look for tag *Step1_CreateProject*


## Add React + ReactRouter
If necessary, change to your project directory `cd MyAwesomeProject`

1- Add react and react-router

`meteor npm install --save react-router@3.0.0 react@15.5.4 react-dom@15.5.4`

2- Edit client/main.html, and replace the <body> content will be:
```
 <body>
    <div id="react-root"></div>
 </body>
```
Whatever the reactRouter decides to show, it will show it in the '#react-root' element

3- Create the Layouts file in imports/ui/layouts/App.jsx
```
import React, { Component } from 'react';
import PropTypes from 'prop-types';


class App extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div>
        {this.props.children}
      </div>
    );
  }
}

App.propTypes = {
  children: PropTypes.node
};

export default App;

```


4- Create the Routes file in imports/startup/client/Routes.jsx
```
import ReactDOM from 'react-dom';
import React, { Component } from 'react';
import { Router, Route, IndexRoute, browserHistory } from 'react-router';

import App from '../../ui/layouts/App.jsx';

import NotFound from '../../ui/pages/NotFound.jsx';
import Index from '../../ui/pages/Index.jsx';


class Routes extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <Router history={ browserHistory }>
        <Route path="/" component={ App }>
          <IndexRoute name="index" component={ Index }/>
          <Route path="*" component={ NotFound }/>
        </Route>
      </Router>
    );
  }
}

Routes.propTypes = {};


Meteor.startup(() =>{
  ReactDOM.render(
    <Routes/>,
    document.getElementById('react-root')
  );
});

```

# Note:
- I'm skipping some other files that you will need to create, to make things shorter. Specifically, check for imports/ui/pages{Index.jsx,NotFound.jsx}.

- You can view a full example in https://github.com/rafa-lft/Meteor_React_Base. Look for tag *Step2_ReactRouter*



## Step 3- Add Accounts
If necessary, change to your project directory `cd MyAwesomeProject`

1- Add accounts packages:
`meteor add accounts-base accounts-password react-meteor-data `

2- Add the routes to *login* and *signup* pages in imports/startup/Routes.jsx 
The *render()* method will be as follows:

```
  render() {
    return (
      <Router history={ browserHistory }>
        <Route path="/" component={ App }>
          <IndexRoute name="index" component={ Index }/>
          <Route name="login" path="/login" component={ Login }/>
          <Route name="signup" path="/signup" component={ Signup }/>
          <Route name="users" path="/users" component={ Users }/>
          <Route name="editUser" path="/users/:userId" component={ EditUser }/>
          <Route path="*" component={ NotFound }/>
        </Route>
      </Router>
    );
  }
```




# Note:
- I'm skipping some other files that you will need, to make things shorter. Specifically, check  imports/startup/server/index.js imports/ui/layouts/{App,NavBar}.jsx and import/ui/pages/{Login,Signup,Users,EditUser}.jsx

- You can view a full example in https://github.com/rafa-lft/Meteor_React_Base. Look for tag *Step3_Accounts*




## Add roles
1- Add roles package ( https://github.com/alanning/meteor-roles)

`meteor add alanning:roles` 

2- Create some roles constants. In file imports/api/accounts/roles.js
```
const ROLES = {
  ROLE1: 'ROLE1',
  ROLE2: 'ROLE2',
  ADMIN: 'ADMIN'
};

export default ROLES;
```

3- I'll not show how to add/update roles on a user, just will mention that on server side, you can set user roles by `Roles.setUserRoles(user.id, roles);` Check for more info in https://github.com/alanning/meteor-roles and http://alanning.github.io/meteor-roles/classes/Roles.html 

4- Assuming you already setup all the accounts and roles files (see full example in https://github.com/rafa-lft/Meteor_React_Base. Look for tag *Step4_roles*) we can now create a method that will be in charge of allowing (or not) access to the different routes.
In imports/startup/client/Routes.jsx

```

class Routes extends Component {
  constructor(props) {
    super(props);
  }

  authenticate(roles, nextState, replace) {
    if (!Meteor.loggingIn() && !Meteor.userId()) {
      replace({
        pathname: '/login',
        state: {nextPathname: nextState.location.pathname}
      });
      return;
    }
    if ('*' === roles) { // allow any logged user
      return;
    }
    let rolesArr = roles;
    if (!_.isArray(roles)) {
      rolesArr = [roles];
    }
    // rolesArr = _.union(rolesArr, [ROLES.ADMIN]);// so ADMIN has access to everything
    if (!Roles.userIsInRole(Meteor.userId(), rolesArr)) {
      replace({
        pathname: '/forbidden',
        state: {nextPathname: nextState.location.pathname}
      });
    }
  }

  render() {
    return (
      <Router history={ browserHistory }>
        <Route path="/" component={ App }>
          <IndexRoute name="index" component={ Index }/>
          <Route name="login" path="/login" component={ Login }/>
          <Route name="signup" path="/signup" component={ Signup }/>

          <Route name="users" path="/users" component={ Users }/>

          <Route name="editUser" path="/users/:userId" component={ EditUser }
                 onEnter={_.partial(this.authenticate, ROLES.ADMIN)} />


          {/* ********************
           Below links are there to show Roles authentication usage.
           Note that you can NOT hide them by
           { Meteor.user() && Roles.userIsInRole(Meteor.user(), ROLES.ROLE1) &&
           <Route name=.....
           }
           as doing so will change the Router component on render(), and ReactRouter will complain with:
           Warning: [react-router] You cannot change <Router routes>; it will be ignored

           Instead, you can/should hide them on the NavBar.jsx component... don't worry: if someone tries to access
           them, they will receive the Forbidden.jsx component
           *************/ }
          <Route name="forAnyOne" path="/for_any_one" component={ ForAnyone }/>

          <Route name="forLoggedOnes" path="/for_logged_ones" component={ ForLoggedOnes }
                 onEnter={_.partial(this.authenticate, '*')} />

          <Route name="forAnyRole" path="/for_any_role" component={ ForAnyRole }
                 onEnter={_.partial(this.authenticate, _.keys(ROLES))}/>

          <Route name="forRole1or2" path="/for_role_1_or_2" component={ ForRole1or2 }
                 onEnter={_.partial(this.authenticate, [ROLES.ROLE1, ROLES.ROLE2])} />

          <Route name="forRole1" path="/for_role1" component={ ForRole1 }
                 onEnter={_.partial(this.authenticate, ROLES.ROLE1)}/>

          <Route name="forRole2" path="/for_role2" component={ ForRole2 }
                 onEnter={_.partial(this.authenticate, ROLES.ROLE2)} />


          <Route name="forbidden" path="/forbidden" component={ Forbidden }/>

          <Route path="*" component={ NotFound }/>
        </Route>
      </Router>
    );
  }
}

```

We added an *onEnter* trigger to some routes. For those routes, we are also passing which Roles are allowed to enter. Note that the onEnter callback, receives 2 params originally. We are using underscore's partial (http://underscorejs.org/#partial), to add another one (roles)
The *authenticate* method (called by onEnter) receives the roles and:
  - Check if the user is logged in at all. If not, redirects to '/login'.
  - If roles === '*' we assume any logged in user can enter, so we allow it
  - Else, we verify if the user is allowed (Roles.userIsInRole) and if not, we redirect to forbidden.
  - Optionally, you can uncomment a line, so ADMIN has access to everything.

The code has several examples of different routes that are allowed for anyone (no onEnter callback), for any logged user, for any logged user with at least 1 role, and for specific roles.

Also note, that ReactRouter (at least on version 3), doesn't allow to modificate the routes on Render. So you can not hide the routes within the Routes.jsx. For that reason, we redirects to /forbidden in the authenticate method.

5- A common bug with ReactRouter and Meteor, relates to user status updates not being shown. For example the user logged out, but we are still showing his/her name on the nav-bar.
That happens because Meteor.user() has changed, but we are not re-rendering. 

That bug can be solved by calling Meteor.user() in the createContainer.
Here is an example of it, used in imports/ui/layouts/NavBar.jsx:

```
export default createContainer((/* {params}*/) =>{
  Meteor.user(); // so we render again in logout or if any change on our User (ie: new roles)
  const loading = !subscription.ready();
  return {subscriptions: [subscription], loading};
}, NavBar);
```



# Note:
- I'm skipping some other files that you will need, to make things shorter. Specifically, check  imports/startup/server/index.js imports/ui/layouts/{App,NavBar}.jsx and import/ui/pages/{Login,Signup,Users,EditUser}.jsx

- You can view a full example in https://github.com/rafa-lft/Meteor_React_Base. Look for tag *Step4_roles*


