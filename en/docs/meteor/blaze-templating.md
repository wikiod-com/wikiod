---
title: "Blaze Templating"
slug: "blaze-templating"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Blaze is a powerful library for creating user interfaces by writing dynamic, reactive HTML templates. Blaze templating allows for loops and conditional logic to be used directly in HTML markup. This section explains and demonstrates the proper usage of templating in Meteor.js with Blaze.

## Populate a template from a method call
<!-- language: lang-spacebars -->

```
<template name="myTemplate">
  {{#each results}}
    <div><span>{{name}}</span><span>{{age}}</span></div>
  {{/each}}
</template>
```

<!-- language: lang-js -->

```
Template.myTemplate.onCreated(function() {
  this.results = new ReactiveVar();
  Meteor.call('myMethod', (error, result) => {
    if (error) {
      // do something with the error
    } else {
      // results is an array of {name, age} objects
      this.results.set(result);
    }
  });
});

Template.myTemplate.helpers({
  results() {
    return Template.instance().results.get();
  }
});
```


## Data context of a template
Whenever a template is called upon, the default data context of the template is implicitly gained from the caller as in example the childTemplate gains the data context of the parentTemplate i.e caller template

<!-- language: lang-spacebars -->

    <template name="parentTemplate">
        {{#with someHelperGettingDataForParentTemplate}}
        <h1>My name is {{firstname}} {{lastname}}</h1>
        //some stuffs here
        {{> childTemplate}}
        {{/with}}
    </template>

In the above situation,whatever data the helper extracts for parent template are automatically gained by childTemplate.For example,the {{firstname}} and  {{lastname}} can be accessed from childTemplate as well as shown below.

<!-- language: lang-spacebars -->

    <template name="childTemplate">
    <h2>My name is also {{firstname}} {{lastname}}</h2>
    </template>

We can even explicitly define the data context of the childTemplate by passsing arguments to the template like in below example.

<!-- language: lang-spacebars -->

    <template name="parentTemplate">
        {{#with someHelperGettingDataForParentTemplate}}
        <h1>My name is {{firstname}} {{lastname}}</h1>
        //some stuffs here
        {{> childTemplate childData=someHeplerReturningDataForChild}}
        {{/with}}
    </template>

Assuming the helper **someHelperReturningDataForChild** returns object like {profession:"Meteor Developer",hobby:"stackoverflowing"},this particular object will be the explicit data context for the childTemplate. Now in child template we can do something like

<!-- language: lang-spacebars -->

    <template name="childTemplate">
        <h2>My profession is {{profession}}</h2>
        <h3>My hobby is {{hobby}}</h3>
    </template>

## Template Helpers
[Template helpers][1] are an essential part of Blaze and provide both business logic and reactivity to a Template.  It is important to remember that Template helpers are actually [reactive computations][2] that are rerun whenever their dependencies change.  Depending on your needs, Template helpers can be defined globally or scoped to a specific template. Examples of each Template helper definition approach is provided below.

 1. Example of a Template helper scoped to a single template.

First define your template:

    <template name="welcomeMessage">
      <h1>Welcome back {{fullName}}</h1>
    </template>

Then define the Template helper.  This assumes that the data context of the template contains a firstName and lastName property.

    Template.welcomeMessage.helpers({
      fullName: function() {
        const instance = Template.instance();
        return instance.data.firstName + ' ' + instance.data.lastName
      },
    });

 2. Example of a global Template helper (this helper can be used from within any Template)

First register the helper:

    Template.registerHelper('equals', function(item1, item2) {
      if (!item1 || !item2) {
        return false;
      }

      return item1 === item2;
    });

With the `equals` helper defined, I can now use it within any template:

    <template name="registration">
      {{#if equals currentUser.registrationStatus 'Pending'}}
        <p>Don't forget to complete your registration!<p>
      {{/if}}
    </template>


  [1]: http://blazejs.org/api/templates.html#Template-helpers
  [2]: http://docs.meteor.com/api/tracker.html#Tracker-autorun

