---
title: "CSS Selectors"
slug: "css-selectors"
draft: false
images: []
weight: 8317
type: docs
toc: true
---

## Syntax
 - by.css('css-selector')
 - by.id('id')
 - by.model('model')
 - by.binding('binding')

## Parameters
| Parameter | Details |
| ------ | ------ |
| css-selector   | A css selector like `'.class-name'` to select the element on the base of class name  |
|id| Id of the dom element|
|model|Model used for dom element|
|binding |Name of the binding which is used to bound to certain element|

**How to write css selectors?**

The most important attributes to write css selectors are class and id of dom. For an instance if a html dom lookes like below example:

      <form class="form-signin">
          <input type="text" id="email" class="form-control" placeholder="Email">
          <input type="password" id="password" class="form-control" placeholder="Password">
          <button class="btn btn-block" id="signin-button" type="submit">Sign in</button>
      </form>

Then to select the email input field, you can write css selector in following way:
1. **Using class name**: The class name in css selector starts with special character .(dot). The css selector for that will be like this `.form-control`.

    `by.css('.form-control')`

Since the `form-control` class is shared by both input elements so it raises a concern of duplicity in locators. So in such situation if id is available then you should always prefer to use id instead of class name.

2. **Using ID**: The id in css selector starts with special character # (hash). So the css selector using id for email input element will be written like below:

    `by.css('#email')`

3. **Using multiple class names**: If dom element has multiple classes then you can with combination of classes as css selector. For example if dom element is like this:


    <input class="username-class form-control">
    // css selector using multiple classes
    by.css('.username-class.form-control')

4. **Using tag name with other attributes** : The general expression to write css selector using tag name and other attributes is `tagname[attribute-type='attribute-vallue']`. So following the expression the css locator for sign-in button can be formed like this: 


    by.css("button[type='submit']") //or
    by.css("button[id='signin-button']")








## $ and $$ CSS selector locator shortcuts
* * *
The Protractor API allows CSS element locators to use the jQuery-like [shortcut notation `$()`][1].     

**Normal CSS Element Locator**: 
       
```javascript
element(by.css('h1.documentation-text[ng-bind="title"]'));
element(by.css('[ng-click="submit"]));
```

**Shortcut `$()` CSS Element Locator**:    

```
$('h1.documentation-text[ng-bind="title"]');
$('[ng-click="submit"]');
```

* * *
For finding multiple elements under a locator use the [shortcut notation `$$()`][2].

**Normal CSS Element Locator**: 
       
```
element.all(by.css('h1.documentation-text[ng-bind="title"]'));
element.all(by.css('[ng-click="submit"]));
```

**Shortcut `$$()` CSS Element Locator**:  
  
```
$$('h1.documentation-text[ng-bind="title"]');
$$('[ng-click="submit"]');
```


  [1]: http://www.protractortest.org/#/api?view=ElementFinder.prototype.$
  [2]: http://www.protractortest.org/#/api?view=ElementFinder.prototype.$$

## Select element by an exact HTML attribute value
To select an element by an exact HTML attribute use the css locator pattern [\[attribute=value\]][1]
```
//selects the first element with href value '/contact'
element(by.css('[href="/contact"]')); 

//selects the first element with tag option and value 'foo'
element(by.css('option[value="foo"]')); 

//selects all input elements nested under the form tag with name attribute 'email'
element.all(by.css('form input[name="email"]'));

```


  [1]: http://www.w3schools.com/cssref/sel_attribute_value.asp

## Select element by an HTML attribute that contains a specified value
To select an element by an HTML attribute that contains a specified value use the css locator pattern [\[attribute*=value\]][1]
```
//selects the first element with href value that contains'cont'
element(by.css('[href*="cont"]')); 

//selects the first element with tag h1 and class attribute that contains 'fo'
element(by.css('h1[class*="fo"]')); 

//selects all li elements with a title attribute that contains 'users'
element.all(by.css('li[title*='users']'));
```


  [1]: http://www.w3schools.com/cssref/sel_attr_contain.asp

## Introduction to locators
A locator in Protractor is used to perform action on HTML dom elements. The most common and best locators used in Protractor are css, id, model and binding. For example commonly used locators are:

    by.css('css-selector')
    by.id('id')

