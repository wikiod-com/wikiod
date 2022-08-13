---
title: "Observables"
slug: "observables"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Explicit Subscription to Observables
    var person = {
      name: ko.observable('John')
    };
    
    console.log(person.name());
    
    console.log('Update name');
    
    person.name.subscribe(function(newValue) {
      console.log("Updated value is " + newValue);
    });
    
    person.name('Jane');



## Creating an observable
**JS**

    // data model
    var person = {
        name: ko.observable('Jack'),
        age: ko.observable(29)
    };
 
    ko.applyBindings(person);

**HTML**

    <div>   
        <p>Name: <input data-bind='value: name' /></p> 
        <p>Age: <input data-bind='value: age' /></p> 
        <h2>Hello, <span data-bind='text: name'> </span>!</h2>  
    </div>

