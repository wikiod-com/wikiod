---
title: "DOM Manipulation"
slug: "dom-manipulation"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

## Creating DOM elements
The `jQuery` function (usually aliased as `$`) can be used both to select elements and to create new elements.

    var myLink = $('<a href="http://stackexchange.com"></a>');

You can optionally pass a second argument with element attributes:

    var myLink = $('<a>', { 'href': 'http://stackexchange.com' });

`'<a>'` --> The first argument specifies the type of DOM element you want to create. In this example it's an [anchor][1] but could be anything [on this list][2].  See the [specification][3] for a reference of the `a` element. 

`{ 'href': 'http://stackexchange.com' }` --> the second argument is a [JavaScript Object][4] containing attribute name/value pairs.

the 'name':'value' pairs will appear between the `<` `>` of the first argument, for example `<a name:value>` which for our example would be `<a href="http://stackexchange.com"></a>`


  [1]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
  [2]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element
  [3]: https://www.w3.org/TR/html5/text-level-semantics.html#the-a-element
  [4]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Grammar_and_types#Object_literals

## Manipulating element classes
Assuming the page includes an HTML element like:

    <p class="small-paragraph">
      This is a small <a href="https://en.wikipedia.org/wiki/Paragraph">paragraph</a>
      with a <a class="trusted" href="http://stackexchange.com">link</a> inside.
    </p>

jQuery provides useful functions to manipulate DOM classes, most notably `hasClass()`, `addClass()`, `removeClass()` and `toggleClass()`. These functions directly modify the `class` attribute of the matched elements.

    $('p').hasClass('small-paragraph'); // true
    $('p').hasClass('large-paragraph'); // false

    // Add a class to all links within paragraphs
    $('p a').addClass('untrusted-link-in-paragraph');

    // Remove the class from a.trusted
    $('a.trusted.untrusted-link-in-paragraph')
    .removeClass('untrusted-link-in-paragraph')
    .addClass('trusted-link-in-paragraph');

**Toggle a class**

Given the example markup, we can add a class with our first `.toggleClass()`:

    $(".small-paragraph").toggleClass("pretty");

Now this would return `true`:
`$(".small-paragraph").hasClass("pretty")`

`toggleClass` provides the same effect with less code as:

    if($(".small-paragraph").hasClass("pretty")){
       $(".small-paragraph").removeClass("pretty");}
    else {
       $(".small-paragraph").addClass("pretty"); }

toggle Two classes:

    $(".small-paragraph").toggleClass("pretty cool");

Boolean to add/remove classes:

    $(".small-paragraph").toggleClass("pretty",true); //cannot be truthy/falsey

    $(".small-paragraph").toggleClass("pretty",false);

Function for class toggle (see example further down to avoid an issue)

    $( "div.surface" ).toggleClass(function() {
      if ( $( this ).parent().is( ".water" ) ) {
        return "wet";
      } else {
        return "dry";
      }
    });

Used in examples:

    // functions to use in examples
    function stringContains(myString, mySubString) {
      return myString.indexOf(mySubString) !== -1;
    }
    function isOdd(num) { return num % 2;}
    var showClass = true; //we want to add the class

Examples:

Use the element index to toggle classes odd/even


    $( "div.gridrow" ).toggleClass(function(index,oldClasses, false), showClass ) {
      showClass
      if ( isOdd(index) ) {
        return "wet";
      } else {
        return "dry";
      }
    });

**More complex `toggleClass` example, given a simple grid markup**

    <div class="grid">
      <div class="gridrow">row</div>
      <div class="gridrow">row</div>
      <div class="gridrow">row</div>
      <div class="gridrow">row</div>
      <div class="gridrow">row</div>
      <div class="gridrow gridfooter">row but I am footer!</div>
    </div>

  Simple functions for our examples:


    function isOdd(num) {
      return num % 2;
    }

    function stringContains(myString, mySubString) {
      return myString.indexOf(mySubString) !== -1;
    }
    var showClass = true; //we want to add the class

  Add an odd/even class to elements with a `gridrow` class

    $("div.gridrow").toggleClass(function(index, oldClasses, showThisClass) {
      if (isOdd(index)) {
        return "odd";
      } else {
        return "even";
      }
      return oldClasses;
    }, showClass);

  If the row has a `gridfooter` class, remove the odd/even classes, keep the rest.


    $("div.gridrow").toggleClass(function(index, oldClasses, showThisClass) {
      var isFooter = stringContains(oldClasses, "gridfooter");
      if (isFooter) {
        oldClasses = oldClasses.replace('even', ' ').replace('odd', ' ');
        $(this).toggleClass("even odd", false);
      }
      return oldClasses;
    }, showClass);

The classes that get returned are what is effected.  Here, if an element does not have a `gridfooter`, add a class for even/odd.  This example illustrates the return of the OLD class list.  If this `else return oldClasses;` is removed, only the new classes get added, thus the row with a `gridfooter` class would have all classes removed had we not returned those old ones - they would have been toggled (removed) otherwise.

 
    $("div.gridrow").toggleClass(function(index, oldClasses, showThisClass) {
      var isFooter = stringContains(oldClasses, "gridfooter");
      if (!isFooter) {
        if (isOdd(index)) {
          return "oddLight";
        } else {
          return "evenLight";
        }
      } else return oldClasses;
    }, showClass);

## Other API Methods
jQuery offers a variety of methods that can be used for DOM manipulation.

The first is the [.empty()][1] method.

Imagine the following markup:

    <div id="content">
      <div>Some text</div>
    </div>

By calling `$('#content').empty();`, the inner div would be removed. This could also be achieved by using `$('#content').html('');`.

Another handy function is the [.closest()][2] function:

    <tr id="row_1">
      <td><button type="button" class="delete">Delete</button>
    </tr>

If you wanted to find the closest row to a button that was clicked within one of the row cells then you could do this:

    $('.delete').click(function() {
      $(this).closest('tr');
    });

Since there will probably be multiple rows, each with their own `delete` buttons, we use `$(this)` within the [.click()][3] function to limit the scope to the button we actually clicked.

If you wanted to get the `id` of the row containing the `Delete` button that you clicked, you could so something like this:

    $('.delete').click(function() {
      var $row = $(this).closest('tr');
      var id = $row.attr('id');
    });

It is usually considered good practise to prefix variables containing jQuery objects with a `$` (dollar sign) to make it clear what the variable is.

An alternative to `.closest()` is the [.parents()][4] method:

    $('.delete').click(function() {
      var $row = $(this).parents('tr');
      var id = $row.attr('id');
    });


and there is also a [.parent()][5] function as well:

    $('.delete').click(function() {
      var $row = $(this).parent().parent();
      var id = $row.attr('id');
    });

`.parent()` only goes up one level of the DOM tree so it is quite inflexible, if you were to change the delete button to be contained within a `span` for example, then the jQuery selector would be broken.


  [1]: https://api.jquery.com/empty/
  [2]: http://api.jquery.com/closest/
  [3]: https://api.jquery.com/click/
  [4]: https://api.jquery.com/parents/
  [5]: https://api.jquery.com/parent/

## Sorting elements
To sort elements efficiently (all at once and with minimal DOM interruption), we need to:

1. **Find** the elements
2. **Sort** based on a set condition
3. **Insert** back in the DOM


    <ul id='my-color-list'>
        <li class="disabled">Red</li>
        <li>Green</li>
        <li class="disabled">Purple</li>
        <li>Orange</li>
    </ul>


1. Find them - `.children()` or `.find()`

   This will give us back an Array-like object to play with.

       var $myColorList = $('#my-color-list');
    
       // Elements one layer deep get .children(), any deeper go with .find()
       var $colors = $myColorList.children('li');

2. Re-arrange them - `Array.prototype.sort()`

   This is currently setup to return the elements in Ascending order based on the HTML content (aka their colors).
   
       /**
        * Bind $colors to the sort method so we don't have to travel up
        * all these properties more than once.
        */
       var sortList = Array.prototype.sort.bind($colors);
   
       sortList(function ( a, b ) {

           // Cache inner content from the first element (a) and the next sibling (b)
           var aText = a.innerHTML;
           var bText = b.innerHTML;
        
           // Returning -1 will place element `a` before element `b`
           if ( aText < bText ) {
               return -1;
           }

           // Returning 1 will do the opposite
           if ( aText > bText ) {
               return 1;
           }

           // Returning 0 leaves them as-is
           return 0;
       });

3. Insert them - `.append()`

    *Note that we don't need to detach the elements first - `append()` will move elements that already exist in the DOM, so we won't have extra copies*

       // Put it right back where we got it
       $myColorList.append($colors);

---

Make it cute
============

Add a sort button
-----------------

    <!-- previous HTML above -->
    <button type='button' id='btn-sort'>
        Sort
    </button>

Set the initial value of sorting direction
------------------------------------------

    var ascending = true;


Cache our DOM elements and `sortList()` out here to minimize our DOM processing
-------------------------------------------------------------------------------

    var $myColorList = $('#my-color-list');
    var $colors = $myColorList.children('li');
    var sortList = Array.prototype.sort.bind($colors);


Wrap everything up in a `doSort()` function
-------------------------------------------
    
    // Put the sortList() and detach/append calls in this portable little thing
    var doSort = function ( ascending ) {
    
        sortList(function ( a, b ) {
            // ...
        });

        $myColorList.append($colors);
    };
    

Add click handler for `$('#btn-sort')`
-----------------

    $('#btn-sort').on('click', function () {
        // Run the sort and pass in the direction value
        doSort(ascending);

        // Toggle direction and save
        ascending = !ascending;
    });

All together now
================

    var ascending = true;

    var $myColorList = $('#my-color-list');
    var $colors = $myColorList.children('li');
    var sortList = Array.prototype.sort.bind($colors);
    
    var doSort = function ( ascending ) {
        
        sortList(function ( a, b ) {
    
            var aText = a.innerHTML;
            var bText = b.innerHTML;
    
            if ( aText < bText ) {
                return ascending ? -1 : 1;
            }
    
            if ( aText > bText ) {
                return ascending ? 1 : -1;
            }
    
            return 0;
        });
        
        $myColorList.append($colors);
    
    };
    
    $('#btn-sort').on('click', function () {
        doSort(ascending);
        ascending = !ascending;
    });

---

**Bonus**

Multi-level sorting (grouping sorted elements)
----------------------------------------------

    // ...

    var doSort = function ( ascending ) {

        sortList(function ( a, b ) {
    
            // ...initial sorting...
    
        }).sort(function ( a, b ) {
            
            // We take the returned items and sort them once more
            var aClass = a.className;
            var bClass = b.className;
            
            // Let's group the disabled items together and put them at the end
    
            /**
             * If the two elements being compared have the same class
             * then there's no need to move anything.
             */
            if ( aClass !== bClass ) {
                return aClass === 'disabled' ? 1 : -1;
            }
            return 0;
        });
    
        // And finalize with re-insert
        $myColorList.append($colors);
    };

    // ...

**Can you take it one step further?**

Add another button to toggle disabled group sorting
-------------------------------------------------------

---
[MDN - Array.prototype.sort()][1]


  [1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort



## .html()
You can use this method to replace all of the HTML within the selector.  Assuming you have an html element like this
    

    <div class="row">
        <div class="col-md-12">
            <div id="information">
                <p>Old message</p>
            </div>
        </div>
    </div>
You could use `.html()`. to remove and add an alert or informational text to alert users all with one line.

    $("#information").html("<p>This is the new alert!</p>");


