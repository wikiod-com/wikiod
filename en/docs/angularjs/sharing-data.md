---
title: "Sharing Data"
slug: "sharing-data"
draft: false
images: []
weight: 9872
type: docs
toc: true
---

A very common question when working with Angular is how to share data between controllers. Using a [service][1] is the most frequent response and this is a simple example demonstrating a [factory][2] pattern to share any type of data object between two or more controllers. Because it is a shared object reference, an update in one controller will be immediately available in all other controllers using the service. Note that both service and factory and both [providers][3].


  [1]: https://www.wikiod.com/angularjs/providers#Service
  [2]: https://www.wikiod.com/angularjs/providers#Factory
  [3]: https://www.wikiod.com/angularjs/providers

## Using ngStorage to share data
Firstly, include the **[ngStorage][1]** source in your index.html.   

An example injecting `ngStorage` src would be:    
  

    <head>
        <title>Angular JS ngStorage</title>
        <script src = "http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
        <script src="https://rawgithub.com/gsklee/ngStorage/master/ngStorage.js"></script>
    </head>

 
`ngStorage` gives you 2 storage namely: `$localStorage` and `$sessionStorage`. You need to require ngStorage and Inject the services.  

Suppose if `ng-app="myApp"`, then you would be injecting `ngStorage` as following:
     

    var app = angular.module('myApp', ['ngStorage']);
            app.controller('controllerOne', function($localStorage,$sessionStorage) {
            // an object to share
            var sampleObject = {
                name: 'angularjs',
                value: 1
            };
           $localStorage.valueToShare = sampleObject;
           $sessionStorage.valueToShare = sampleObject;
        })  
    .controller('controllerTwo', function($localStorage,$sessionStorage) {
        console.log('localStorage: '+ $localStorage +'sessionStorage: '+$sessionStorage);
    })

`$localStorage` and `$sessionStorage` is globally accessible through any controllers as long as you inject those services in the controllers.  

You can also use the `localStorage` and `sessionStorage` of `HTML5`. However, using `HTML5` `localStorage` would require you to serialize and deserialize your objects before using or saving them.    

**For example:**

    var myObj = {
        firstname: "Nic",
        lastname: "Raboy",
        website: "https://www.google.com"
    } 
    //if you wanted to save into localStorage, serialize it  
    window.localStorage.set("saved", JSON.stringify(myObj));  
    
    //unserialize to get object  
    var myObj = JSON.parse(window.localStorage.get("saved"));

 


  [1]: https://github.com/gsklee/ngStorage

## Sharing data from one controller to another using service
We can create a `service` to `set` and `get` the data between the `controllers` and then inject that service in the controller function where we want to use it.

**Service :**

    app.service('setGetData', function() {
      var data = '';
        getData: function() { return data; },
        setData: function(requestData) { data = requestData; }
    });

**Controllers :**

    app.controller('myCtrl1', ['setGetData',function(setGetData) {
    
      // To set the data from the one controller
      var data = 'Hello World !!';  
      setGetData.setData(data);
    
    }]);

    app.controller('myCtrl2', ['setGetData',function(setGetData) {
    
      // To get the data from the another controller  
      var res = setGetData.getData();
      console.log(res); // Hello World !!
    
    }]);

Here, we can see that `myCtrl1` is used for `setting` the data and `myCtrl2` is used for `getting` the data. So, we can share the data from one controller to another contrller like this.

