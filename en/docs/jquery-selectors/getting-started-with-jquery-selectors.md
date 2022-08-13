---
title: "Getting started with jquery-selectors"
slug: "getting-started-with-jquery-selectors"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
1    Tag Name
Represents a tag name available in the DOM. For example `$('p')` selects all paragraphs `<p>` in the document.

2    Tag ID
Represents a tag available with the given ID in the DOM. For example `$('#some-id')` selects the single element in the document that has an ID of some-id.

3    Tag Class
Represents a tag available with the given class in the DOM. For example `$('.some-class')` selects all elements in the document that have a class of some-class.

All the above items can be used either on their own or in combination with other selectors. All the jQuery selectors are based on the same principle except some tweaking.

NOTE − The factory `function $()` is a synonym of `jQuery()` function. So in case you are using any other JavaScript library where $ sign is conflicting with some thing else then you can replace $ sign by jQuery name and you can use function jQuery() instead of $().

Example
Following is a simple example which makes use of Tag Selector. This would select all the elements with a tag name p and will set their background to "yellow".


    <html>
    
       <head>
          <title>The jQuery Example</title>
          <script type = "text/javascript" 
             src = "http://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script>
    
          <script type = "text/javascript" language = "javascript">
             $(document).ready(function() {
                $("p").css("background-color", "yellow");
             });
          </script>
       </head>
        
       <body>
          <div>
             <p class = "myclass">This is a paragraph.</p>
             <p id = "myid">This is second paragraph.</p>
             <p>This is third paragraph.</p>
          </div>
       </body>
        
    </html>

## Jquery Selectors
Jquery selectors are used to manipulate DOM (Document object model), attach events, add element, remove element on runtime.

| Selector | Description |
| ------ | ------ |
| element selector   | element selector are used to select particular element <br/>Ex: `<p>Stackoverflow to help in understanding errors </p>` <br>To access this "p" tag, we need to add this element and wrap it inside jquery syntax like : $("p"). <br> **"$"** represents the jquery, its just aliasing of jquery and if we want we can use jquery instead of "$" like **jQuery()**. <br> If we are working with other library or framework (angular) that has same aliasing as jquery ($), we can alter this conflict by using $.noConflict() method of jquery.     |
|ID selector | we can write Id selector as $("#text of id attribute") <br> `<div id="selectMe">I am inner content of Div.</div>` <br> Here text of **ID** attribute is "selectMe", so to select this ID selector using jquery we have to write: $("#selectMe"). <br> Multiple IDs can be selected using comma seperate Ex: $("#id1,#id2,#id3...") |
|Class Selector| Class selectors is represented by dot/"." and written as $(".className")<br> `<span class="demoClass"> Demo JQuery class Selectors </span>` <br> To access class selector, we write $(".demoClass"), Multiple class can be selected as comman separated values. $(".class1,.class2,.class3")|
|All element selector | To select Complete DOM element, we have to include character : `"*"`. <br> `$("*")` It is referencing complete DOM element including html, head... <br> 
    <p> This is p Tag </p>
    <div> This is div Tag</div>
    <span> This is span Tag</span>
    <script>$("*") // It will refer all the DOM element</script>
|

Check jsfiddle for example: https://jsfiddle.net/rezjvrum/

List of various selectors:

 1. ":first"      -  Select first element.
 2. ":last"       -  Select last element.
 3. ":even"       -  Select all even element.
 4. ":odd"        -  Select all odd element.
 5. ":eq(index)"  -  Select the indexed element. Ex: `:eq(1)` will select second child element. Here indexing is starts from "0".
 6. ":gt(index)"  - Show all element whose index is greater than the index pass. Ex: `:gt(3)` will return all the element having index greater than 3.
7. ":lt(index)"   - Show all element whose index is less than the index pass. Ex: `:lt(2)` will return all element having index less than 2.

Above selectors are covered in : https://jsfiddle.net/rezjvrum/2/

