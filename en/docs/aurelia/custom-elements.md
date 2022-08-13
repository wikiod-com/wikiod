---
title: "Custom Elements"
slug: "custom-elements"
draft: false
images: []
weight: 9909
type: docs
toc: true
---

A Custom Element in Aurelia is used to extend the basic set of HTML elements by feature-enriched, reusable components. A Custom Element normally exists out of two files, a View-Model based on Javasciprt, and a corresponding view written in HTML. Both files compose the HTML element which can then be used throughout the application like every other HTML element.

## Creating A Custom Element Based On Naming Conventions
A basic custom element is created in Aurelia based on naming conventions, by simply adding the suffix `CustomElement` to the name of a class. This suffix will automatically be stripped out by Aurelia. The remaining part of the class name will be lowercased and separated using a hyphen and can then be used as the element name.

**Example**:
***my-element.js***

```
export class SuperCoolCustomElement {

}
```

```
<template>
    <h1>I am a custom element</h1>
</template>
```

**Using it:**

To use the newly defined custom element, at first, the tag name needs to be retrieved from the class name. The `CustomElement` suffix will be stripped out and the remaining part (`SuperCool`) will lowercased and separated by hyphen on each capital letter. Hence, `SuperCoolCustomElement` becomes `super-cool` and forms the basis of our element.

```
<template>
    <require from="./my-element"></require>

    <super-cool></super-cool>
</template>
```

Worth noting is that our example above is a bit contrived. We could have just created a HTML-only Custom Element and not create a viewmodel at all. However, the viewmodel approach approach allows you to provide bindable properties to make your element configurable (as shown in other examples).

## Creating A Custom Element With Bindable Properties
Creating a Custom Element with bindable properties is a snap. If you want to create an element that accepts one or more values which the plugin can use, the `@bindable` decorator and syntax is what you are looking for.

Below, we are creating a custom element that accepts an array of fruits and displays them.

**Example:** ***my-element.js***

```
import {bindable} from 'aurelia-framework';

const validFruits = [
    'Apple', 
    'Banana', 
    'Orange', 
    'Pineapple', 
    'Grapes', 
    'Peach',
    'Plum',
    'Dates',
    'Strawberries'
];

export class FruitCustomElement {
    @bindable fruits = [];

    fruitsChanged(newVal, oldVal) {
        if (newVal) {
            newVal.filter(fruit => {
                return validFruits.indexOf(fruit) >= 0;
            });
        }
    }
}
```

```
<template>
    <ul if.bind="fruits">
        <li repeat.for="fruit of fruits">${fruit}</li>
    </ul>
</template>
```

**Using it:**

```
export class MyViewModel {
    myFruits = [];

    attached() {
        this.myFruits = ['Apple', 'Banana', 'Orange', 'Pineapple', 'Grapes', 'Peach'];
    }
}
```

```
<template>
    <require from="./my-element"></require>

    <fruit fruits.bind="myFruits"></fruit>
</template>
```

## Creating A Custom Element Using the @customElement Decorator
In most examples, the class naming convention is used to define an Aurelia Custom Element. However, Aurelia also provides a decorator that can be used to decorate a class. The class is again treated as a custom element by Aurelia then.

The value supplied to the decorator becomes the name of the custom HTML element.

**Example:** ***my-element.js***

```
import {customElement} from 'aurelia-framework';

@customElement('robots')
export class MyClass {

}
```

```
<template>
    <h1>I am a robots custom element</h1>
</template>
```

**Using it:**

```
<template>
    <require from="./my-element"></require>

    <robots></robots>
</template>
```



## HTML Only Custom Element With Bindable Parameters
In the following, we are creating an example of an Aurelia Custom Element which will allow you to display Youtube videos via their video ID. 

An Aurelia Custom Element can be defined in two different ways: 
the first one is by creating a viewmodel and accompanying view, the second one is by just creating a basic HTML file and using the `bindable` property on the `<template>` tag of the view itself.

For our example below, an HTML-only Custom Element makes sense as we are just making it easy to use Youtube embed code in our application.

**Example:** ***youtube.html***

```
<template bindable="videoId">
<iframe width="560" height="315" src="https://www.youtube.com/embed/${videoId}" frameborder="0" allowfullscreen></iframe>
</template>
```

The filename for an HTML-only Custom Element is what will be used as the tag name in our HTML. Therefor, ensure that you don't call it something generic like `header.html`, `footer.html` or any other name that is a native HTML element already.

**Using it:**

```
<template>
    <require from="./youtube.html"></require>

    <youtube video-id="C9GTEsNf_GU"></youtube>
</template>
```

## Javascript Only Custom Element
A Custom Element consisting of Javascript only includes the corresponding HTML view within the `@inlineView` decorator of Aurelia.

The following example takes two bindable paramters, a prename and a surename, and display both within a predefined sentence.

**Example:** ***my-element.js***

```
import { bindable, customElement, inlineView } from 'aurelia-framework';

@customElement('greeter')
@inlineView(`
  <template>
    <b>Hello, \${prename} \${surename}.</b>
  </template>
`)
export class Greeter {
  @bindable prename;
  @bindable surename;
}
```

**Using it:**
```
<template>
  <require from="./greeter"></require>
  <greeter prename="Michael" surename="Mueller"></greeter>
</template>
```

This will output "**Hello, Michael Mueller.**" in the browser's window.

