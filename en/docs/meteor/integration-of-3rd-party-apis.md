---
title: "Integration of 3rd Party APIs"
slug: "integration-of-3rd-party-apis"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Basic HTTP Call
Conceptually, integrate 3rd party REST APIs can be as simple as adding the ``http`` package and making a call to the external endpoint. 

```
meteor add http
```

```
HTTP.get('http://foo.net/api/bar/');
```

## Create A Package For Your API Wrapper
Basic HTTP calls don't provide code-reusability, however.  And they can get confused with all the other features you're trying to implement.  For those reasons, it's common to implement an API wrapper.  

```
Foo = {
  identify: function(input){
    return Http.get('http://foo.net/api/identify/' + input);    
  },
  record_action_on_item: function(firstInput, secondInput){
    return Http.put('http://foo.net/api/record_action_on_item/' + firstInput + '&' + secondInput);    
  }
}
```

Meteor supports Http.get(), Http.post(), Http.put(), etc, so that's undoubtably the best way to call your REST API. 
http://docs.meteor.com/#http_get

If the API is chatty and verbose, you may receive multiple packets; in which case you'll need to reassemble them. This is a big hassle. If you think the API is returning multiple packets, you're probably going to want to use the 'request' npm module on the server. You'll want to use a ``Npm.require('request')``.
https://github.com/mikeal/request

## Create an Atmosphere Package For Your API Wrapper
After creating an API wrapper, it's likely that you may want to create an Atmosphere package to redistribute it and share it between applications. The files of your package will probably look something like this.

```
packages/foo-api-wrapper/package.js
packages/foo-api-wrapper/readme.md
packages/foo-api-wrapper/foo.api.wrapper.js
```

In particular, your ``foo-api-wrapper/package.js`` file will want to look something like this:

```
Package.describe({
  summary: "Atmosphere package that impliments the Foo API.",
  name: "myaccount:foo",
  version: '0.0.1'
});

Package.on_use(function (api) {
    api.export('Foo');
    api.addFiles('foo.api.wrapper.js', ["client","server"]);
});
```

And your ``foo-api-wrapper/foo.api.wrapper.js`` should contain the ``Foo`` API wrapper object.

## Include the API Package in your Application
At this point, you're still building your package, so you'll need to add the package to your application:

```
meteor add myaccount:foo
```

And eventually publish it to Atmosphere:

```
meteor publish myaccount:foo
```

## Using the API Wrapper Object in your App
Now that we have all those pieces put together, you should now be able to make calls like the following from within your app:

```
Foo.identify('John');
Foo.record_action_on_item('view', "HackerNews');
```

Obviously you'll want to adjust function names, arguments, urls, and the like, to create the proper syntax for the API.

