---
title: "Traversal"
slug: "traversal"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Tree walking


## Iterating over nodes
The [NodeIterator](https://developer.mozilla.org/en-US/docs/Web/API/NodeIterator) interface provides methods for iterating over nodes in a DOM tree.

Given a document like this one:

<!-- language: lang-html --->

    <html>
    <body>
      <section class="main">
        <ul>
          <li>List Item</li>
          <li>List Item</li>
          <li>List Item</li>
          <li>List Item</li>
        </ul>
      </section>
    </body>
    </html>

One could imagine an iterator to get the `<li>` elements:

<!-- language: lang-js --->

    let root = document.body;
    let whatToShow = NodeFilter.SHOW_ELEMENT | NodeFilter.SHOW_TEXT;
    let filter = (node) => node.nodeName.toLowerCase() === 'li' ? 
      NodeFilter.FILTER_ACCEPT : 
      NodeFilter.FILTER_REJECT;
    let iterator = document.createNodeIterator(root, whatToShow, filter);
    var node;
    while (node = iterator.nextNode()) {
      console.log(node);
    }

*Example adapted from the example provided by the [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Document/createNodeIterator$history) from the [`document.createNodeIterator()`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createNodeIterator) documentation on the Mozilla Developer Network, licensed under [CC-by-SA 2.5](http://creativecommons.org/licenses/by-sa/2.5/).*

This will log something like:

<!-- language: lang-html --->

    <li>List Item</li>
    <li>List Item</li>
    <li>List Item</li>
    <li>List Item</li>

Note that this is similar to the [TreeWalker](https://www.wikiod.com/dom) iterface, but provides only `nextNode()` and `previousNode()` functionality.


