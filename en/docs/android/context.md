---
title: "Context"
slug: "context"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

Per Google documentation: "Interface to global information about an application environment. It allows access to application-specific resources and classes, as well as up-calls for application-level operations such as launching activities, broadcasting and receiving intents, etc." 

More simply put, Context is the current state of your application. It allows you to provide information to objects so that they can be aware of what is going on in other parts of your application. 




## Syntax
 - `getApplicationContext()`
 - `getBaseContext()`
 - `getContext()`
 - `this`

This StackOverflow page has several comprehensive and well written explanations of the concept of Context:

[What is Context?][1]


  [1]: http://stackoverflow.com/questions/3572463/what-is-context-on-android

## Basic Examples
Standard usage in Activity:  
  

    Context context = getApplicationContext();

Standard usage in Fragment:

    Context context = getActivity().getApplicationContext(); 

 

`this` (when in a class that extends from Context, such as the Application, Activity, Service and IntentService classes)

    TextView textView = new TextView(this);

another `this` example:
   
    Intent intent = new Intent(this, MainActivity.class);
    startActivity(intent);




