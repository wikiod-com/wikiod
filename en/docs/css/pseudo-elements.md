---
title: "Pseudo-Elements"
slug: "pseudo-elements"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

Pseudo-elements, just like pseudo-classes, are added to a CSS selectors but instead of describing a special state, they allow you to scope and style certain parts of an html element.

For example, the ::first-letter pseudo-element targets only the first letter of a block element specified by the selector.

## Syntax
- selector::pseudo-element {property: value}

## Parameters
| pseudo-element | Description |
| -------------- | ----------- |
| `::after` | Insert content after the content of an element  |
| `::before` | Insert content before the content of an element |
| `::first-letter` | Selects the first letter of each element |
| `::first-line` | Selects the first line of each element |
| `::selection` | Matches the portion of an element that is selected by a user |
| `::backdrop` | Used to create a backdrop that hides the underlying document for an element in the top layer's stack  |
| `::placeholder` | Allows you to style the placeholder text of a form element (Experimental) |
| `::marker` | For applying list-style attributes on a given element (Experimental) |
| `::spelling-error` | Represents a text segment which the browser has flagged as incorrectly spelled (Experimental) |
| `::grammar-error` | Represents a text segment which the browser has flagged as grammatically incorrect (Experimental) |

 - Sometimes you will see double colons (`::`) instead of just one (`:`). This is a way to separate pseudo-classes from pseudo-elements, but some older browsers like Internet Explorer 8 **only** supports single colon (`:`) for pseudo-elements. 

 - One can use only one pseudo-element in a selector. It must appear after the simple selectors in the statement.

- Pseudo-elements are not a part of the DOM, therefore are not targetable by `:hover` or other user events. 


## Pseudo-Elements
Pseudo-elements are added to selectors but instead of describing a special state, they allow you to style certain parts of a document.

The `content` attribute is required for pseudo-elements to render; however, the attribute can have an empty value (e.g. `content: ""`).

<!-- language: lang-css --> 
  
    div::after {
      content: 'after';
      color: red;
      border: 1px solid red;
    }
    
    div {
      color: black;
      border: 1px solid black;
      padding: 1px;
    }
    
    div::before {
      content: 'before';
      color: green;
      border: 1px solid green;
    }


[![Result of code][1]][1]


  [1]: http://i.stack.imgur.com/5Lu08.png

## Pseudo-Elements in Lists
Pseudo-elements are often used to change the look of lists (mostly for unordered lists, ``ul``).

The first step is to remove the default list bullets:

<!-- language: lang-css -->

    ul {
      list-style-type: none;
    }
Then you add the custom styling. In this example, we will create gradient boxes for bullets.

<!-- language: lang-css -->

    li:before {
      content: "";
      display: inline-block;
      margin-right: 10px; 
      height: 10px;
      width: 10px;
      background: linear-gradient(red, blue);
    }

**HTML**
```
<ul>
   <li>Test I</li>
   <li>Test II</li>
</ul>
```
**Result**

[![Result][1]][1]


  [1]: http://i.stack.imgur.com/y47uU.png

