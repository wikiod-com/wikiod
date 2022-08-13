---
title: "React Routing"
slug: "react-routing"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Example Routes.js file, followed by use of Router Link in component
**Place a file like the following in your top level directory. It defines which components to render for which paths**

```javascript
import React from 'react';
import { Route, IndexRoute } from 'react-router';
import New from './containers/new-post';
import Show from './containers/show';

import Index from './containers/home';
import App from './components/app';

export default(
  <Route path="/" component={App}>
    <IndexRoute component={Index} />
    <Route path="posts/new" component={New} />
    <Route path="posts/:id" component={Show} />

  </Route>
);

```

**Now in your top level index.js that is your entry point to the app, you need only render this Router component like so:**

```javascript
import React from 'react';
import ReactDOM from 'react-dom';
import { Router, browserHistory } from 'react-router';
// import the routes component we created in routes.js
import routes from './routes';


// entry point
ReactDOM.render(
    <Router history={browserHistory} routes={routes} />
  , document.getElementById('main'));
```

**Now it is simply a matter of using `Link` instead of `<a>` tags throughout your application. Using Link will communicate with React Router to change the React Router route to the specified link, which will in turn render the correct component as defined in routes.js**

``` javascript
import React from 'react';
import { Link } from 'react-router';

export default function PostButton(props) {
  return (
    <Link to={`posts/${props.postId}`}>
      <div className="post-button" >
        {props.title}
        <span>{props.tags}</span>
      </div>
    </Link>
  );
}
```


## React Routing Async
    import React from 'react';
    import { Route, IndexRoute } from 'react-router';
    
    import Index from './containers/home';
    import App from './components/app';
    
    //for single Component lazy load use this
    const ContactComponent = () => {
      return {
          getComponent: (location, callback)=> {
            require.ensure([], require => {
              callback(null, require('./components/Contact')["default"]);
            }, 'Contact');
          }
        }
    };
   
    //for multiple componnets
     const groupedComponents = (pageName) => {
      return {
          getComponent: (location, callback)=> {
            require.ensure([], require => {
              switch(pageName){
                case 'about' :
                    callback(null, require( "./components/about" )["default"]);
                        break ;
                case 'tos' :
                    callback(null, require( "./components/tos" )["default"]);
                        break ;
              }
            }, "groupedComponents");
          }
        }
    };
    export default(
      <Route path="/" component={App}>
        <IndexRoute component={Index} />
        <Route path="/contact" {...ContactComponent()} />
        <Route path="/about" {...groupedComponents('about')} />
        <Route path="/tos" {...groupedComponents('tos')} />
      </Route>
    );

