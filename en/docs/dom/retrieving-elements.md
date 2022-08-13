---
title: "Retrieving Elements"
slug: "retrieving-elements"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## By ID
    document.getElementById('uniqueID')

will retrieve

    <div id="uniqueID"></div>

As long as an element with the given ID exists, `document.getElementById` will return only that element. Otherwise, it will return `null`.

**Note:** IDs must be unique. Multiple elements cannot have the same ID.

## By Tag Name
    document.getElementsByTagName('b')

will retrieve

    <b>All</b>
    <b>of</b>
    <b>the b elements.</b>

If no elements with the given tag name exist, an empty collection will be returned.

## By CSS Selector
Consider following html code

    <ul>
      <li id=“one” class=“main”>Item 1</li>
      <li id=“two” class=“main”>Item 2</li>
      <li id=“three” class=“main”>Item 3</li>
      <li id=“four”>Item 4</li>
    </ul>

Following dom tree will be constructed based on above html code

 

                          ul
        
                          |
        
         |          |         |         |
        
        li         li        li        li
         |          |         |         |
    Item 1        Item 2    Item 3    Item 4

We can select elements from DOM tree with the help of CSS selectors. This is possible by means of two javascript methods viz `querySelector()` and `querySelectorAll()`.

**querySelector()** method returns the first element that matches the given css selector from the DOM.
        
    document.querySelector('li.main')

returns the first `li` element who's class is `main`

    document.querySelector('#two')
returns the element with id `two`

**NOTE:**  If no element is found `null` is returned. If the selector string contains a CSS pseudo-element, the return will be `null`.

----------
 
**querySelectorAll()** method returns all the elements that matches the given css selector from the DOM.

    document.querySelectorAll('li.main')

returns a node list containing all the `li` elements who's class is `main`.


**NOTE**:  If no element is found an empty node list is returned. If the selectors string contains a CSS pseudo-element, the returned elementList will be empty



## Query Selectors
In modern browsers [[1]][caniuse-queryselector], it is possible to use CSS-like selector to query for elements in a document -- the same way as [sizzle.js][sizzle-js] (used by jQuery).
 
# querySelector
 
Returns the first [`Element`][api-element] in the document that matches the query. If there is no match, returns `null`.
 

    // gets the element whose id="some-id"
    var el1 = document.querySelector('#some-id');
     
    // gets the first element in the document containing "class-name" in attribute class
    var el2 = document.querySelector('.class-name');
     
    // gets the first anchor element in the document
    var el2 = document.querySelector('a');
     
    // gets the first anchor element inside a section element in the document
    var el2 = document.querySelector('section a');
     
# querySelectorAll
 
Returns a [`NodeList`][api-nodelist] containing all the elements in the document that match the query. If none match, returns an empty `NodeList`.
 

    // gets all elements in the document containing "class-name" in attribute class
    var el2 = document.querySelectorAll('.class-name');
     
    // gets all anchor elements in the document
    var el2 = document.querySelectorAll('a');
    
    // gets all anchor elements inside any section element in the document
    var el2 = document.querySelectorAll('section a');

 [sizzle-js]: //sizzlejs.com/
 [caniuse-queryselector]: //caniuse.com/#search=querySelector
 [api-element]: https://developer.mozilla.org/en-US/docs/Web/API/element
 [api-nodelist]: https://developer.mozilla.org/en-US/docs/Web/API/NodeList


## By Class Name
    document.getElementsByClassName('class-name')

will retrieve

    <a class="class-name">Any</a>
    <b class="class-name">tag</b>
    <div class="class-name an-extra-class">with that class.</div>

If no existing elements contain the given class, an empty collection will be returned.

----------

**Example:**

    <p class="my-class">I will be matched</p>
    <p class="my-class another-class">So will I</p>
    <p class="something-else">I won't</p>

<!-- language: lang-js -->

    var myClassElements = document.getElementByClassName('my-class');
    console.log(myClassElements.length); // 2
    var nonExistentClassElements = document.getElementByClassName('nope');
    console.log(nonExistentClassElements.length); // 0

