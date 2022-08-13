---
title: "Manipulating Elements"
slug: "manipulating-elements"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Cloning elements


## Adding an element
In this example we create a new list element with the text "new text", and select the first unordered list, and its first list element.

    let newElement = document.createElement("li");
    newElement.innerHTML = "new text";

    let parentElement = document.querySelector("ul");
    let nextSibling = parentElement.querySelector("li");

When inserting an element, we do it *under* the parent element, and just before a particular child element of that parent element.

    parentElement.insertBefore(newElement, nextSibling);

> The new element is inserted under `parentElement` and just before `nextSibling`.

When one wants to insert an element as the last child element of `parentElement`, the second argument can be `null`.

    parentElement.insertBefore(newElement, null);

> The new element is inserted under `parentElement` as the last child.

Instead, `appendChild()` may be used to simply append the child to the children of the parent node.

    parentElement.appendChild(newElement);

> The new element is inserted under `parentElement` as the last child.



## Replacing an element
In this example we create a new list element with the text "new text", and select the first unordered list, and its first list element.

    let newElement = document.createElement("li");
    newElement.innerHTML = "new text";

    let parentElement = document.querySelector("ul");
    let nextSibling = parentElement.querySelector("li");

To replace an element, we use `replaceChild`:

    parentElement.replaceChild(newElement, nextSibling);

> `nextSibling` is removed from the DOM. In its place is now `newElement`.

## Removing an element
An element can be removed by calling `remove()` on it. Alternatively, one can call `removeChild()` on its parent. `removeChild()` has better browser support than `remove()`.

    element.remove();

> `element`, and all its childnodes, are removed from the DOM.

    parentElement.removeChild(element);

> `element`, and all its childnodes, are removed from the DOM.

In any case, one can insert this node in the DOM at a later point in time as long as there are still references to this node.

## Append and Prepend methods
JavaScript now have the Append and Prepend methods which was present in jQuery

The main advantage of `append` and `prepend` is unlike `appendChild` and `insertBefore`, it can take any number of arguments either HTML element or plain text(which will be converted to text nodes).

To append say 1 div, 1 text node and 1 span

    document.body.append(document.createElement('div'),"Hello world",document.createElement('span'))

This will change the page to the following structure

    <body>
          .....(other elements)
          <div></div>
          "Hello World"
          <span></span>
    </body>

To prepend the same in body

Use

    document.body.prepend(document.createElement('div'),"Hello world",document.createElement('span'))

This will change the page to the following structure

    <body>
          <div></div>
          "Hello World"
          <span></span>
          .....(other elements)
    </body>


Note that browser supports are

Chrome 54+  
Firefox 49+  
Opera 39+

Read more at MDN

[Append](https://developer.mozilla.org/en-US/docs/Web/API/ParentNode/append)

[Prepend](https://developer.mozilla.org/en-US/docs/Web/API/ParentNode/prepend)

