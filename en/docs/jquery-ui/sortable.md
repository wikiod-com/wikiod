---
title: "Sortable"
slug: "sortable"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
   - $("#sortable").sortable({ /\*Options Here*/ }); //Initialise Sortable

   - $("#sortable").sortable("option", "option_name", option_value); //Set option outside initialiser

   - var value = $("#sortable").sortable("option", "option_name"); //Gets the value of an option

## Parameters
| Parameter | Description |
| ----- | ----- |
| **Options** | &nbsp; |
| appendTo | (jQuery, Element, Selector, String) [Default: "parent"] The element that the helper is added to |
| axis | (String) [Default: false] The directions that the item can be dragged (x or y) |
| cancel | (Selector) [Default: "input,textarea,button,select,option"] Doesnt start sorting if you start on an element matching the selector |
| classes | (Object) [Default: {}] Specify additional classes to add to the sortables elements when the structural classes are added. ({ui-sortable-helper: custom_class}) |
| connectWith | (Selector) [Default: false] Allows to items from one sortable to be dragged to another |
| containment | (Element, Selector, String) [Default: false] The element that items are constrained to |
| cursor | (String) [Default: "auto"] Defines the type of cursor to be shown when sorting |
| cursorAt | (Object) [Default: false] Defines the position that the helper looks like its being moved from |
| disabled | (Boolean) [Default: false] Disables the sorting if true |
| dropOnEmpty | (Boolean) [Default: true] If false items from this sortable can not be placed in empty sortables |
| forceHelperSize | (Boolean) [Default: false] Forces the helper to have a size |
| forcePlaceholderSize | (Boolean) [Default: false] Forces the placeholder to have a size |
| grid | (Array) [Default: false] Defines a grid to snap the helper to ([ x,y ]) |
| handle | (Selector, Element) [Default: false] Defines elements that sorting can start on. Opposite to cancel |
| helper | (String, Function) [Default: "original"] String "original" or "clone", or function that returns the element to be used as the helper. |
| items | (Selector) [Default: "> *"] Defines the items that should be sortable |
| opacity | (Number 0.01 to 1) [Default: false] Defines the opacity for the helper|
| placeholder | (String) [Default: false] Defines a class or classes to be applied to the placeholder |
| revert | (Boolean, Number) [Default: false] The time that it takes the helper to slide into its new position |
| scroll | (Boolean) [Default: true] Whether to scroll when at edges of the page |
| scrollSensitivity | (Number) [Default: 20] Defines how close to the edge of the page the cursor needs to be to start scrolling |
| scrollSpeed | (Number) [Default: 20] The speed at which to scroll |
| tolerance | (String) [Default: "intersect"] Defines which mode to use when calculating when one item is over another ("intersect" or "pointer") |
| zIndex | (Integer) [Default: 1000] Defines the z-index of the helper when sorting |
|**Methods**| &nbsp; |
| cancel() | Cancels the current sort and returns the elements back to their position before the sort started |
| destroy() | Removes the sortable functionality and returns the element to its state pre initialisation |
| disable() | Disables the sortable |
| enable() | Enables the sortable |
| instance() | Returns the sortables instance object |
| option() | Gets key value pairs of all the options for the sortable |
| option(String) | Gets the value of an option |
| option(String, Any) | Sets the value of the option specified by the String |
| option(Object) | Sets one or more options with the object being key value pairs of options |
| refresh() | Refreshes the sortable options reloading all sortable items. This causes new items to be recognised |
| refreshPositions() | Rrefreses the cached positions of the sortable items |
| serialize(Object) | Serializes the items ids (by default) into a string that can be submitted or appended to a url Object options:  {key: sets the key in the serialized string, attribute:[Default "id"] sets the attribute to look at, expression:[Default: "/(.+)[-=_](.+)/"] regex to split the attribute in to key value pairs}|
| toArray(Object) | Serializes the sortable items id into an array. The object can contain a parameter attribute which has the attribute to put into the array default is id | 
| widget() | Returns a jQuery object of the sortable element |
|**Events**| &nbsp; |
| activate(event, ui) | Triggered when connected list, every connected list on drag start receives it |
| beforeStop(event, ui) | Triggers before sorting stops when the helper has being shiftered to the same position as the placeholder |
| change(event, ui) | Triggered when elements change position ie. when the placeholder moves |
| create(event, ui) | Triggered when sortable is created |
| deactivate(event, ui) | Triggered when sorting stops. This goes to all connected lists as well |
| out(event, ui) | Triggered when the item is moved out of the sortable list |
| over(event, ui) | Triggered when the item is moved into a sortable list |
| receive(event, ui) | Triggered when an item from a connected list has being dropped into another one. Target is the receiving list |
| remove(event, ui) | Triggered when an item from a connected list has being dropped into another one. Target is the giving list |
| sort(event, ui) | Triggered during sorting |
| start(event, ui) | Triggered when sorting starts |
| stop(event, ui) | Triggered when sorting stops |
| update(event, ui) |Triggered when sorting stops and the DOM position has being updated |




Official Documentation [here][1]


  [1]: http://api.jqueryui.com/sortable/#option-appendTo

## Sortable Grid with flex layout
This used the flex layout with the sortable to create a grid of responsive boxes that can be moved around by dragging and dropping.

HTML

    <div id="sortable">
      <div>1</div>
      <div>2</div>
      <div>3</div>
      <div>4</div>
      <div>5</div>
    </div> 

JS

    $(function(){
        $('#sortable').sortable({
            //pass all options in here
        });
    });

CSS

    #sortable{
        width: 500px;
        display: flex;
        flex-wrap: wrap;
    }
    #sortable div {
        margin: 10px;
        background-color: #f00;
        flex-basis: 100px;
        height: 100px;
    }

## Stationary Items when dragging
This example uses a class on the placeholder to turn it into a line and make it take up no room.

HTML

    <div id="sortable">
        <div>1</div>
        <div>2</div>
        <div>3</div>
        <div>4</div>
    </div>

JS

    $("#sortable").sortable({
        placeholder: 'placeholder',
        helper: 'clone',
        start: function(event, ui){
            ui.item.show();
        }
    });

CSS

    #sortable div{
      background-color: #f00;
      width: 50px;
      height: 50px;
      margin: 10px;
      padding: 0px;
    }
    #sortable div.placeholder{
      height: 4px;
      margin: -7px 10px;
    }



## Sortable - Animate revert of unaccepted item
Working Example: https://jsfiddle.net/Twisty/4f5yh3pa/7/

Cancelling and Reverting a sortable is not strongly documented. The helps show how moving an item from one list to another connected list can be conditionally cancelled. by default, this is not animated by sortable, this example includes an animation.

Result: List #2 only accepts items that have a class of `acceptable`. Both lists can be sorted naturally otherwise.

**HTML**

    <div class="ui-widget">
      <ul id="sortable1" class="connectedSortable">
        <li class="ui-state-default acceptable">Item 1</li>
        <li class="ui-state-default">Item 2</li>
        <li class="ui-state-default">Item 3</li>
        <li class="ui-state-default">Item 4</li>
        <li class="ui-state-default">Item 5</li>
      </ul>
      <ul id="sortable2" class="connectedSortable">
        <li class="ui-state-default">Item 6</li>
        <li class="ui-state-default acceptable">Item 7</li>
      </ul>
    </div>

**CSS**

    .ui-widget {
      position: relative;
    }
    
    .connectedSortable {
      border: 1px solid #eee;
      width: 142px;
      min-height: 20px;
      list-style-type: none;
      margin: 0;
      padding: 5px 0 0 0;
      float: left;
      margin-right: 10px;
    }
    
    #sortable1 {
      background: #fff;
    }
    
    #sortable2 {
      background: #999;
    }
    
    .connectedSortable li {
      margin: 0 5px 5px 5px;
      padding: 5px;
      font-size: 1.2em;
      width: 120px;
    }

**JavaScript**

    $(function() {
      $(".connectedSortable").sortable({
        connectWith: ".connectedSortable",
        receive: function(e, ui) {
          var $self = $(this);
          var $item = ui.item;
          var $sender = ui.sender;
          // Restrict condition to only one specific list if desired
          if ($(e.target).attr("id") == "sortable2") {
            if ($item.hasClass("acceptable")) {
              // Item Accepted
              console.log($self.attr("id") + " accepted item from: #" + $sender.attr("id") + " > " + $item.text());
            } else {
              // Item Rejected
              console.log($self.attr("id") + " rejected item from: #" + $sender.attr("id") + " > " + $item.text());
              // Animate the return of the items position
              $item.css("position", "absolute").animate(ui.originalPosition, "slow", function() {
                // Return the items position control to it's parent
                $item.css("position", "inherit");
                // Cancel the sortable action to return it to it's origin
                $sender.sortable("cancel");
              });
            }
          }
        }
      }).disableSelection();
    });



## Simple Example
Take any list and add an identifier to the outer wrapper (`ul`, `div`)
 
    <ul id="sortable">
    <li>Item 1</li>
    <li>Item 2</li>
    <li>Item 3</li>
    <li>Item 4</li>
    </ul>

In your jquery: 

    $(function(){
        $('#sortable').sortable({
            //pass all options in here
        });
    });


This will allow all the `li` in the `#sortable` wrapper to be dragged and dropped in the list



