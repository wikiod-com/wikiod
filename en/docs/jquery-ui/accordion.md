---
title: "Accordion"
slug: "accordion"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Syntax
 - $(function() {
    $( "#selecter" ).accordion();
}); 
 - $(function() {
    $( "#selecter" ).accordion({
      active: 2
    });
}); 
 - $(function() {
    $( "#selecter" ).accordion({
          animate: 200
    });
});
 - $(function() {
    $( "#selecter" ).accordion({
          collapsible: true
    });
}); 

## Parameters
| Parameter | Detail |
| ------ | ------ |
| active     | Type Boolean or Integer, Boolean requires collapsible to be true    |
| animate             | Type Boolean, Number, String or Object    | 
| collapsible     | Type     Boolean        |           
| event | Type     String    |    
| header     | Type     Selector element        |
| heightStyle | Type     String                |         
| icons             | Type     jQuery UI icon object    |       

More information can be found here: http://api.jqueryui.com/accordion/

## Accordion Basic Usage
To use an accordion, one must have headers and content inside the headers in their HTML. Then one must instantiate the `accordion()` method of jQuery UI.

    <script>
    $(function() {
        $( "#accordion" ).accordion();
    });
    </script>

In the HTML:

    <div id="accordion">
        <h3>First header</h3>
            <div>First content panel</div>
        <h3>Second header</h3>
            <div>Second content panel</div>
    </div>

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/1i4uV.png

## Accordion destroy usage
    $( "#accordion" ).accordion( "destroy" );

This will remove the accordion functionality completely and show default HTML removing all the jQuery-UI elements.

This method does not take any arguments.    

## Accordion disable Usage
    $( "#accordion" ).accordion( "disable" );

This method will disable the accordion, i.e. the headers are not selectable making the content read only and static.
    
This method does not take any arguments.

## Accordion enable Usage
    $( "#accordion" ).accordion( "enable" );

This method will enable an accordion. This will enable a disabled accordion or simply do nothing on an already enabled accordion.  

This method does not take any arguments.

## Accordion option Usage
    var options = $( "#accordion" ).accordion( "option" );

This will return a PlainObject giving all the options representing the selected accordion. This will contain all the values of the keys that are explained in the Parameters section.

This method takes parameters which are the basic optionNames explained in the parameter. One can set the options like this:

    $( "#accordion" ).accordion( "option", "disabled", true );    
    

## Accordion refresh Usage
    $( "#accordion" ).accordion( "refresh" );

This method recomputes the height of the accordion panels if headers or content was added or removed in the DOM.

## Accordiong widget usage
    var widget = $( "#accordion" ).accordion( "widget" );

This method returns a jQuery object containing the accordion.

