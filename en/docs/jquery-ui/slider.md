---
title: "Slider"
slug: "slider"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Using the Slide Event
The slider provides an event called `slide` that will trigger **whenever the mouse moves during a slider handle drag**.  This function holds a reference to the slide `event` and a reference to the slider `ui` object.  The `ui` object holds a jQuery object for the handle being moved and the value(s) of the slider.

**Single-Handle Slider:**

    var value;
    
    $( "#slider" ).slider({
        slide: function(event, ui) {
          value = ui.value;
        }
    });

**Range Slider:**

    var lowValue;
    var highValue;
    
    $( "#range-slider" ).slider({
        range: true,
        slide: function(event, ui) {
          lowValue = ui.values[0];
          highValue = ui.values[1];
        }
    });

**Note:** The `slide` event is intended to respond to active mouse motion and will not trigger if the slider values are changed programmatically.  To react to these events, use the `change` event.

## Setting Values and the Change Event
The slider provides an event called `change` that will trigger **after the mouse completes a slider handle drag or if the value(s) have been changed programmatically**.  This function holds a reference to the slide `event` and a reference to the slider `ui` object. The `ui` object holds a jQuery object for the handle being moved and the value(s) of the slider.

One example could be having to display new information after a slider's values have been updated by another element's event.  Let's use a `select` element for demonstration where the value of the slider is programmatically set in when the value of the `select` changes:

**HTML**

    <select id="setting">
      <option value="1">Low</option>
      <option value="2">Medium</option>
      <option value="3">High</option>
    </select>
    
    <div id="slider"></div>
    
    <div id="display-value"></div>

**JavaScript**

    $(function() {
      $( "#slider" ).slider({
        min: 0,
        max: 11,
        // This will trigger when the value is programmatically changed
        change: function(event, ui) {
            $( "#display-value" ).text(ui.value);
        }
      });
      
      $( "#setting" ).change(function () {
        switch ($(this).val()) {
          case "1":
            $( "#slider" ).slider( "value", 3 );  // Sets the value of a slider programmatically
            break;
          case "2":
            $( "#slider" ).slider( "value", 7 );  // Sets the value of a slider programmatically
            break;
          case "3":
            $( "#slider" ).slider( "value", 11 ); // Sets the value of a slider programmatically
            break;
        }
      });
    });

**Note:** It's in these circumstances that the `slide` event would not trigger and the `change` event is needed.  However, if elements need to react to the slider values changing as the handle is being dragged, the `slide` event will be necessary.

## Simple Example
A slider control uses draggable handles to select numeric values.  Below is an example of a basic slider initialization:

    <script>
      $(function() {
        $( "#slider" ).slider();
      });
    </script>
    <div id="slider"></div>

## Range Slider
Range sliders provide 2 draggable handles to select numeric values.  The slider's initialization must provide a `range` option set to `true` to create a range slider:

    <script>
      $(function() {
        $( "#range-slider" ).slider({
            range: true
        });
      });
    </script>
    <div id="range-slider"></div>

## Initializing Values and Value Limits
A slider element can have its value set on initialization by providing a `value` option.  This option is a number:

    $( "#slider" ).slider({
        value: 5
    });

A range slider can also have its values set in this way by providing a `values` option.  This option is an array of numbers:

    $( "#range-slider" ).slider({
        range: true,
        values: [5, 25]
    });

In addition to providing initial values, the minimum value, maximum value, and handle interval can be defined with the `min`, `max`, and `step` options, respectively:

    $( "#range-slider" ).slider({
        range: true,
        min: 0,    // The lowest possible value will be 0
        max: 100,  // The highest possible value will be 100
        step: 5,   // The slider handles will lock in at intervals of 5
        values: [0, 100]
    });

