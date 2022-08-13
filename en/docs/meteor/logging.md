---
title: "Logging"
slug: "logging"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Basic Server Side Logging
The first step to logging is simply to run Meteor from the shell, and you'll get the server logs in the command console.

```
meteor
```

The next step is to pipe the contents of std_out and std_err to a logfile, like so:

```
meteor > my_app_log.log 2> my_app_err.log
```






## Client Side Logging Tools
Once you have your server side logging in place, it's time to hop over to the client side. If you haven't explored the console API, be prepared for a treat. There's actually all sorts of things that you can do with the built in Console API that's native to every Chrome and Safari installation. So much so, in fact, that you may find yourself not needing Winston or other logging frameworks.

The first thing you'll want to do is install client side logging and developer tools. Chrome and Safari both ship with them, but Firefox requires the Firebug extension.

[Firebug Extension](https://addons.mozilla.org/en-US/firefox/addon/firebug/)  

Then, you'll want to check out the Console API documentation. The following two documents are invaluable resources for learning console logging.

[Chrome Developer Tools](https://developers.google.com/chrome-developer-tools/docs/console)  
    
[Firebug (Client)](http://getfirebug.com/logging)  



## Advanced Server Logging Tools
Once you have both your server-side logging running, and your client side development tools, you can start looking at Meteor specific extensions like the Meteor Chrome DevTools Extension. This lets you actually observe server logging in the client! Because the database is everywhere. As is logging.

[Chrome DevTools Extension (Server)](https://github.com/gandev-de/meteor-server-console)  


## Logging error on database flap
The following example is from 0.5 - 0.7 days, and illustrates how to log an error when the database hasn't populated the client side cursor yet.  

```
Template.landingPage.postsList = function(){
  try{
    return Posts.find();
  }catch(error){
    //color code the error (red)
    console.error(error);
  }
}
```

## Logging info on the data context in a template helper
The following uses the Chrome Logging API.  If the ``.group()`` syntax is used in multiple templates, it will graphically organize the console logs from different templates into a hierarchical tree.  

You can also see how to inspect the current data context, and how to stringify data.

```
Template.landingPage.getId = function(){
  // using a group block to illustrate function scoping
  console.group('coolFunction');

  // inspect the current data object that landingPage is using
  console.log(this);

  // inspect a specific field of the locally scoped data object
  console.log(JSON.stringify(this._id);

  // close the function scope
  console.groupEnd();
  return this._id;
}
```

## Logging events and user interactions  
Simple example of using the Chrome Logging API.  

```
Template.landingPage.events({
  'click .selectItemButton':function(){
    // color code and count the user interaction (blue)
    console.count('click .selectItemButton');
  }
});
```

## Logging with log level variables
Logging can often clutter up the console, so it's common to define log levels to control what detail of data is getting logged.  A common pattern is to specify a log level variables.

```
var DEBUG = false;
var TRACE = false;
Template.landingPage.events({
  'click .selectItemButton':function(){
    TRACE && console.count('click .selectItemButton');

    Meteor.call('niftyAction', function(errorMessage, result){
        if(errorMessage){
            DEBUG && console.error(errorMessage);    
        }
    });
  }
});
```


## Disable Logging in Production
Some teams find that they want to leave console log statements in their code, but not have them display in production.  They will override the logging functions if a variable isn't set (possibly an environment variable).  Additionally, this may qualify as a security feature in some situations.  

```
if (!DEBUG_MODE_ON) {
    console = console || {};
    console.log = function(){};

    console.log = function(){};
    console.error = function(){};
    console.count = function(){};
    console.info = function(){};
}
```

## Winston
If you need something more powerful than the default logging options, you might want to look at a tool like Winston. Go to Atmosphere, and simply search for one of the many Winston packages available. 

https://atmospherejs.com/?q=winston

Be warned, however - Winston is a sophisticated product, and while it exposes a lot of functionality, it will also add a layer of complexity to your application.

## Loglevel
A special mention should be made for the community developed LogLevel package. It appears to strike a balance between being lightweight and simple to use, while working well with Meteor's bundle pipeline and preserving line numbers and filenames. 

https://atmospherejs.com/practicalmeteor/loglevel



