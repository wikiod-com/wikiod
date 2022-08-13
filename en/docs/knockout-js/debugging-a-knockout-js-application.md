---
title: "Debugging a knockout.js application"
slug: "debugging-a-knockoutjs-application"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Checking the binding context of a DOM element
Many bugs in knockout data binds are caused by undefined properties in a viewmodel. Knockout has two handy methods to retrieve the [binding context][1] of an HTML element:

<!-- lang: js -->
    // Returns the binding context to which an HTMLElement is bound
    ko.contextFor(element);    

    // Returns the viewmodel to which an HTMLElement is bound
    // similar to: ko.contextFor(element).$data
    ko.dataFor(element);       

To quickly find out the binding context of a UI element, here's a handy trick:

Most modern browsers store the currently selected DOM element in a global variable: `$0` ([more about this mechanism][2])

 - Right click an element in your UI and choose _"inspect"_ or _"inspect element"_ in the context menu.
 - type `ko.dataFor($0)` in the developer console and press enter

Browser plugins also exist which may assist with finding the object context.

An example (try it on [Knockout hello world example][3]):

[![A gif showing how to quickly log knockout's binding context for a UI element][4]][4]


  [1]: http://knockoutjs.com/documentation/binding-context.html
  [2]: https://www.wikiod.com/javascript/debugging#Interactive interpreter variables
  [3]: http://knockoutjs.com/examples/helloWorld.html "Knockout hello world example"
  [4]: http://i.stack.imgur.com/dHP6G.gif

## Printing a binding context from markup
Sometimes it is useful to print a current binding directly from markup. A neat trick which allows that is to use an additional DOM element with a non-existing binding (KO < 3.0), custom binding or a binding that is not relevant such as `uniqueName`. 

Consider this example: 

    <tbody data-bind="foreach: people">
        <tr>
             <td data-bind="text: firstName"></td>
             <td data-bind="text: lastName"></td>
        </tr>
    </tbody>

If one would like to to find out the binding context of each element in the people array, one can write: 

    <tbody data-bind="foreach: people">
        <span data-bind="uniqueName: console.log($data)"></span>
        <tr>
            <td data-bind="text: firstName"></td>
            <td data-bind="text: lastName"></td>
        </tr>
    </tbody>

