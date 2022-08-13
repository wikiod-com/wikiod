---
title: "Events"
slug: "events"
draft: false
images: []
weight: 9899
type: docs
toc: true
---

jQuery internally handles events via the [addEventListener][1] function. This means it is perfectly legal to have more than one function bound to the same event for the same DOM element.


  [1]: https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener

## Delegated Events
Let's start with example.
Here is a very simple example HTML.

<h1>Example HTML</h1>

    <html>
        <head>
        </head>
        <body>
            <ul>
                <li>
                    <a href="some_url/">Link 1</a>
                </li>
                <li>
                    <a href="some_url/">Link 2</a>
                </li>
                <li>
                    <a href="some_url/">Link 3</a>
                </li>
            </ul>
        </body>
    </html>

----

<h1>The problem</h1>

Now in this example, we want to add an event listener to all `<a>` elements.
The problem is that the list in this example is dynamic.
`<li>` elements are added and removed as time passes by.
However, the page does not refresh between changes, which would allow us to use simple click event listeners to the link objects (i.e. `$('a').click()`).

The problem we have is how to add events to the `<a>` elements that come and go.

----

<h1>Background information - Event propagation</h1>

Delegated events are only possible because of event propagation (often called event bubbling). Any time an event is fired, it will bubble all the way up (to the document root). They *delegate* the handling of an event to a non-changing ancestor element, hence the name "delegated" events.

So in example above, clicking `<a>` element link will trigger 'click' event in these elements in this order:
- a
- li
- ul
- body
- html
- document root

----

<h1>Solution</h1>

Knowing what event bubbling does, we can catch one of the wanted events which are propagating up through our HTML.

A good place for catching it in this example is the `<ul>` element, as that element does is not dynamic:

    $('ul').on('click', 'a', function () {
      console.log(this.href); // jQuery binds the event function to the targeted DOM element
                              // this way `this` refers to the anchor and not to the list
      // Whatever you want to do when link is clicked
    });

In above:
- We have 'ul' which is the recipient of this event listener
- The first parameter ('click') defines which events we are trying to detect.
- The second parameter ('a') is used to declare where the event needs to *originate* from (of all child elements under this event listener's recipient, ul).
- Lastly, the third parameter is the code that is run if first and second parameters' requirements are fulfilled.

<h1>In detail how solution works</h1>

1. User clicks `<a>` element
2. That triggers click event on `<a>` element.
3. The event start bubbling up towards document root.
4. The event bubbles first to the `<li>` element and then to the `<ul>` element.
5. The event listener is run as the `<ul>` element has the event listener attached.
6. The event listener first detects the triggering event. The bubbling event is 'click' and the listener has 'click', it is a pass.
7. The listener checks tries to match the second parameter ('a') to each item in the bubble chain. As the last item in the chain is an 'a' this matches the filter and this is a pass too.
8. The code in third parameter is run using the matched item as it's `this`. If the function does not include a call to `stopPropagation()`, the event will continue propagating upwards towards the root (`document`).

Note: If a suitable non-changing ancestor is not available/convenient, you should use `document`. As a habit do not use `'body'` for the following reasons:

 - `body` has a bug, to do with styling, that can mean mouse events do not bubble to it. This is browser dependant and can happen when the calculated body height is 0 (e.g. when all child elements have absolute positions). Mouse events always bubble to `document`.
 - `document` *always* exists to your script, so you can attach delegated handlers to `document` outside of a *DOM-ready handler* and be certain they will still work.


## Attach and Detach Event Handlers
Attach an Event Handler
=======================

Since version [1.7][1] jQuery has the event API `.on()`. This way any [standard javascript event][2] or custom event can be bound on the currently selected jQuery element. There are shortcuts such as `.click()`, but `.on()` gives you more options. 


----------


HTML
----

    <button id="foo">bar</button>

jQuery
------

    $( "#foo" ).on( "click", function() {
      console.log( $( this ).text() ); //bar
    });


----------


Detach an Event Handler
=======================

Naturally you have the possibility to detach events from your jQuery objects too. You do so by using `.off( events [, selector ] [, handler ] )`. 


----------


HTML
----

    <button id="hello">hello</button>

jQuery
------

    $('#hello').on('click', function(){
        console.log('hello world!');
        $(this).off();
    });

When clicking the button `$(this)` will refer to the current jQuery object and will remove all attached event handlers from it. You can also specify which event handler should be removed. 

jQuery
------

    $('#hello').on('click', function(){
        console.log('hello world!');
        $(this).off('click');
    });
    
    $('#hello').on('mouseenter', function(){
        console.log('you are about to click');
    });

In this case the `mouseenter` event will still function after clicking.

  [1]: http://api.jquery.com/category/version/1.7/
  [2]: https://developer.mozilla.org/en-US/docs/Web/Events

## Document Loading Event .load()
If you want your script to wait until a certain resource was loaded, such as an image or a PDF you can use `.load()`, which is a shortcut for shortcut for `.on( "load", handler)`.

**HTML**

    <img src="image.jpeg" alt="image" id="image">

**jQuery**

    $( "#image" ).load(function() {
      // run script
    });

## Events for repeating elements without using ID's
**Problem**

There is a series of repeating elements in page that you need to know which one an event occurred on to do something with that specific instance. 

**Solution**

 - Give all common elements a common class
 - Apply event listener to a class.  `this` inside event handler is the matching selector element the event occurred on
 - Traverse to outer most repeating container for that instance by starting at `this`
 - Use `find()` within that container to isolate other elements specific to that instance

HTML

    <div class="item-wrapper" data-item_id="346">
       <div class="item"><span class="person">Fred</span></div>
       <div class="item-toolbar">
          <button class="delete">Delete</button>
       </div>   
    </div>
    <div class="item-wrapper" data-item_id="393">
       <div clss="item"><span class="person">Wilma</span></div>
       <div class="item-toolbar">
          <button class="delete">Delete</button>
       </div>   
    </div>

jQuery

    $(function() {
      $('.delete').on('click', function() {
        // "this" is element event occured on
        var $btn = $(this);
        // traverse to wrapper container
        var $itemWrap = $btn.closest('.item-wrapper');
        // look within wrapper to get person for this button instance
        var person = $itemWrap.find('.person').text();
        // send delete to server and remove from page on success of ajax
        $.post('url/string', { id: $itemWrap.data('item_id')}).done(function(response) {
          $itemWrap.remove()
        }).fail(function() {
          alert('Ooops, not deleted at server');
        });
      });
    });




## originalEvent
Sometimes there will be properties that aren't available in jQuery event. To access the underlying properties use `Event.originalEvent`

# Get Scroll Direction

    $(document).on("wheel",function(e){
        console.log(e.originalEvent.deltaY)
        // Returns a value between -100 and 100 depending on the direction you are scrolling
    })


## Switching specific events on and off via jQuery. (Named Listeners)
Sometimes you want to switch off all previously registered listeners.

    //Adding a normal click handler
    $(document).on("click",function(){
        console.log("Document Clicked 1")
    });
    //Adding another click handler
    $(document).on("click",function(){
        console.log("Document Clicked 2")
    });
    //Removing all registered handlers.
    $(document).off("click")
    
An issue with this method is that ALL listeners binded on `document` by other plugins etc would also be removed.

**More often than not, we want to detach all listeners attached only by us.**

To achieve this, we can bind named listeners as,
    
    //Add named event listener.
    $(document).on("click.mymodule",function(){
        console.log("Document Clicked 1")
    });
    $(document).on("click.mymodule",function(){
        console.log("Document Clicked 2")
    });

    //Remove named event listener.
    $(document).off("click.mymodule");

This ensures that any other click listener is not inadvertently modified.

