---
title: "Usage of Extension Library"
slug: "usage-of-extension-library"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Here you find some tipps and use case examples for the "Extension Library" of OpenNTF (http://extlib.openntf.org/).

This works at least for IE 11.
I didn't test this for other browsers.

## Use a "Font Awesome Icon" for the DatePicker (xe:djDateTextBox)
The default icon of the Date-Picker in the Extension Library from OpenNTF is nice but not sexy. You can change it to one of the cool "Font Awesome Icons".

First, add a DatePicker component to your page:

    <xe:djDateTextBox id="datePickerComp" value="#{myDoc.myDateField}" />

Then add these lines to your custom CSS file:

    
    /*
       ---------------------------------------------------- 
       Improve xe:djDateTextBox Elements with another icon.
       ---------------------------------------------------- 
    */
    
    .dijitSelect .dijitArrowButton{
        padding:0px;
    }

    /* hide default arrow icon: */
    .dijitDateTextBox .dijitArrowButtonInner{
        display: none;
    }

    .dijitDateTextBox .dijitArrowButtonContainer:after{
        font: 12px/normal FontAwesome; 
        content: "\f073"; /* <--- unicode of the icon */
        font-size-adjust: none; 
        font-stretch: normal;
    }
    /* ---------------------------------------------------- */

The value of attribute **content** is the "Icon-Code" (in this example "`\f073`" is the simple "fa-calendar" icon).

If you want another icon, visit http://fontawesome.io/icons/, search for your favorite icon, click to open ist detail, and there copy the Unicode. Paste the Unicode into yout custom CSS file as value of attribute content.

