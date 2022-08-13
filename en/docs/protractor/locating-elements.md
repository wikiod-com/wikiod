---
title: "Locating Elements"
slug: "locating-elements"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

To be able to interact with a page, you need to tell Protractor exactly which element to look for. The basis used for selecting elements are locators. Protractor, as well as including the generic Selenium selectors, also has Angular-specific locators which are more robust and persistent to changes. However, sometimes, even in an Angular application, regular locators must be used.


## Parameters
| Parameter| Detail|
| ------ | ------ |
| selector| A string which specifies the value of the selector (depends on the locator)|

## Protractor specific locators (for Angular-based applications)
These locators should be used as a priority when possible, because they are more persistent to changes in an application then locators based on css or xpath, which can easily break. 

Binding locator
===============

**Syntax**

    by.binding('bind value')

Example
-------

**View**

    <span>{{user.password}}</span>
    <span ng-bind="user.email"></span>

**Locator**

    by.binding('user.password')
    by.binding('user.email')

Also supports partial matches

    by.binding('email')

Exact Binding locator
=====================

Similar to `binding`, except partial matches are not allowed.

**Syntax**

    by.exactBinding('exact bind value')

Example
-------

**View**

    <span>{{user.password}}</span>

**Locator**

    by.exactBinding('user.password')
    by.exactBinding('password') // Will not work

Model locator
=============

Selects an element with an [Angular model directive][1]

**Syntax**

    by.model('model value')

Example
-------

**View**

    <input ng-model="user.username">

**Locator**

    by.model('user.username')

Button text locator
===================

Selects a button based on its text. Should be used only if button text not expected to change often. 

**Syntax**

    by.buttonText('button text')

Example
-------

**View**

    <button>Sign In</button>

**Locator**

    by.buttonText('Sign In')

Partial button text locator
===========================

Similar to `buttonText`, but allows partial matches. Should be used only if button text not expected to change often. 

**Syntax**

    by.partialButtonText('partial button text')

Example

**View**

    <button>Register an account</button>

**Locator**

    by.partialButtonText('Register')

Repeater locator
================

Selects an element with an [Angular repeater directive][2]

**Syntax**

    by.repeater('repeater value')

Example
-------

**View**

    <tbody ng-repeat="review in reviews">
        <tr>Movie was good</tr>
        <tr>Movie was ok</tr>
        <tr>Movie was bad</tr>
    </tbody>

**Locator**
    
    by.repeater('review in reviews')
    
Also supports partial matches

    by.repeater('reviews')

Exact repeater locator
======================

Similar to `repeater`, but does not allow partial matches

**Syntax**

    by.exactRepeater('exact repeater value')

Example
-------

**View**

    <tbody ng-repeat="review in reviews">
        <tr>Movie was good</tr>
        <tr>Movie was ok</tr>
        <tr>Movie was bad</tr>
    </tbody>

**Locator**

    by.exactRepeater('review in reviews')
    by.exactRepeater('reviews') // Won't work

CSS and text locator
====================

An extended CSS locator where you can also specify the text content of the element.

**Syntax**

    by.cssContainingText('css selector', 'text of css element')

Example
-------

**View**

    <ul>
        <li class="users">Mike</li>
        <li class="users">Rebecca</li>
    </ul>

**Locator**

    by.cssContainingText('.users', 'Rebecca') // Will return the second li only

Options locator
===============

Selects an element with an [Angular options directive][3]

**Syntax**

    by.options('options value')

Example
-------

**View**

    <select ng-options="country.name for c in countries">
        <option>Canada</option>
        <option>United States</option>
        <option>Mexico</option>
    </select>

**Locator**
    
    by.options('country.name for c in countries')

Deep CSS locator
================

CSS locator that extends into the [shadow DOM][4]

**Syntax**

    by.deepCss('css selector')

Example
-------

**View**

    <div>
        <span id="outerspan">
        <"shadow tree">
            <span id="span1"></span>
            <"shadow tree">
                <span id="span2"></span>
            </>
        </>
    </div>

**Locator**
    
    by.deepCss('span') // Will select every span element


  [1]: https://docs.angularjs.org/api/ng/directive/ngModel
  [2]: https://docs.angularjs.org/api/ng/directive/ngRepeat
  [3]: https://docs.angularjs.org/api/ng/directive/ngOptions
  [4]: https://glazkov.com/2011/01/14/what-the-heck-is-shadow-dom/

## Locator basics
Locators by themselves do not return an element which can be interacted with in Protractor, they are simply instructions that indicate Protractor how to find the element.

To access the element itself, use this syntax:

    element(locator);
    element.all(locator);

*Note: the element(s) is not actually accessed until an action is performed on it - that is, Protractor will only actually go retrieve the element when an action such as getText() is called on the element.*

If you want to select only one element using a locator, use `element`. If your locator points to multiple elements, `element` will return the first one found. `element` returns an `ElementFinder`. 

If you want to select multiple elements using a locator, `element.all` will return all elements found. `element.all` returns an `ElementArrayFinder`, and every element in the array can be accessed using different methods - for example, the `map` function. 

    element.all(locator).map(function(singleElement) {
            return singleElement.getText();
        }
    });


----------


**Chaining locators**

You can chain multiple locators to select an element in a complex application. You can't directly chain `locator` objects, you must chain `ElementFinders`:

    element(by.repeater('movie in movies').element(by.linkText('Watch Frozen on Netflix')

There is no limit to how many you chains you can use; in the end, you will still recieve a single `ElementFinder` or and `ElementArrayFinder`, depending on your locators.



