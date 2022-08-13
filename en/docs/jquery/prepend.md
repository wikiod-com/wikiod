---
title: "Prepend"
slug: "prepend"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Prepending an element to a container
**Solution 1:**

     $('#parent').prepend($('#child')); 

 

**Solution 2:**

     $('#child').prependTo($('#parent'));

Both solutions are prepending the element `#child` (adding at the beginning) to the element `#parent`.

Before:

    <div id="parent">
      <span>other content</span>
    </div>
    <div id="child">
    
    </div>

After:

    <div id="parent">
      <div id="child">
    
      </div>
      <span>other content</span>
    </div>

## Prepend method

[`prepend()`][1] - Insert content, specified by the parameter, to the beginning of each element in the set of matched elements.
     
__1.__ [`prepend( content [, content ] )`][1]

    // with html string
    jQuery('#parent').prepend('<span>child</span>');
    // or you can use jQuery object
    jQuery('#parent').prepend($('#child'));
    // or you can use comma seperated multiple elements to prepend
    jQuery('#parent').prepend($('#child1'),$('#child2'));


__2.__ [`prepend(function)`][1]


JQuery _`version: 1.4`_  onwards you can use callback function as the argument. Where you can  get arguments as index position of the element in the set and the old HTML value of the element. Within the function, `this` refers to the current element in the set.


    jQuery('#parent').prepend(function(i,oldHTML){      
         // return the value to be prepend
         return  '<span>child</span>';
    });


  [1]: http://api.jquery.com/prepend/

