---
title: "Buttons in twitter-bootstrap-3"
slug: "buttons-in-twitter-bootstrap-3"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Using twitter-bootstrap `btn` class
`btn` class of Twitter-bootstrap can be used with any of the following html elements.

 1. anchor
 2. button
 3. input `with both type="button" and type="submit"`

Below are examples of all possible use cases of `btn` class
    
    
    <a class="btn" href="#" role="button">Link</a>
    <button class="btn" type="submit">Button</button>
    <input class="btn" type="button" value="Input">
    <input class="btn" type="submit" value="Submit">
Although `btn` class can used in any of the above four ways but it is highly recommend using the `<button>` element whenever possible

Image of the above code is attached below

[![enter image description here][2]][2]

 [Source][1]

  [1]: http://getbootstrap.com/css/#buttons
  [2]: http://i.stack.imgur.com/jrq5V.png

## different sizes of button in twitter-bootstrap-3
twitter-bootstrap-3 has provided four different sizes of buttons

 1. Large button `btn-lg`
 2. Default button `does not require any btn size`
 3. Small button `btn-sm`
 4. Extra small button `btn-xs`


     <button type="button" class="btn  btn-lg">Large button</button>
     <button type="button" class="btn">Default button</button>
     <button type="button" class="btn  btn-sm">Small button</button>
     <button type="button" class="btn  btn-xs">Extra small button</button>
   

Image of the above code is attached below 
[![enter image description here][1]][1]

[Source][2]

  [1]: http://i.stack.imgur.com/RifrS.png
  [2]: http://getbootstrap.com/css/#buttons


## Add Glyphicon to Button
Glyphicons can be used in text, buttons, toolbars, navigation, forms, etc (Source: W3Schools). Glyphicons are basically icon forms that can be used to style any of the aforementioned. These examples outline the usage of glyphicons inside two types of buttons by simply using a span inside the buttons which have a class of type glyphicon:

HTML Button

    <button type="button" class="btn btn-info">
        <span class="glyphicon glyphicon-search"></span> Search
    </button>

[![Search Button][1]][1]

ASP Button

    <asp:LinkButton runat="server" CssClass="btn btn-info" >
        <span class="glyphicon glyphicon-envelope"></span> Email
    </asp:LinkButton>

[![Email Button][2]][2]


  [1]: https://i.stack.imgur.com/vFCGh.png
  [2]: https://i.stack.imgur.com/xoYdV.png

