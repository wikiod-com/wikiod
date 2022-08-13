---
title: "Attributes"
slug: "attributes"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

The jQuery function `.attr()`, gets the value of an attribute for the **first** element in the set of matched elements or set one or more attributes for **every** matched element.

It is worth noting that when getting the value of an attribute, it only gets it from the first element that matches the selector (i.e. `$("input").attr("type");` would only get the type of the first input, if there are more than one)

However, when setting an attribute, it will apply it to all matching elements.

## Differece between attr() and prop()
[`attr()`][1] gets/sets the HTML attribute using the DOM functions `getAttribute()` and `setAttribute()`. [`prop()`][2] works by setting the DOM property without changing the attribute. In many cases the two are interchangeable, but occasionally one is needed over the other.

To set a checkbox as checked:

    $('#tosAccept').prop('checked', true); // using attr() won't work properly here

To remove a property you can use the [`removeProp()`][3] method. Similarly [`removeAttr()`][4] removes attributes.


  [1]: https://www.wikiod.com/jquery/attributes#Get the attribute value of a HTML element
  [2]: https://api.jquery.com/prop/
  [3]: https://api.jquery.com/removeProp/
  [4]: https://www.wikiod.com/jquery/attributes#Removing attribute

## Get the attribute value of a HTML element
When a single parameter is passed to the [`.attr()`][1] function it returns the value of passed attribute on the selected element.

Syntax:

`$([selector]).attr([attribute name]);`

Example:

HTML:

`<a href="/home">Home</a>`

jQuery:

`$('a').attr('href');`

**Fetching `data` attributes:**

jQuery offers [`.data()`][2] function in order to deal with data attributes. `.data` function returns the value of the data attribute on the selected element.

Syntax:

`$([selector]).data([attribute name]);`

Example:

Html:

`<article data-column="3"></article>`

jQuery:

`$("article").data("column")`


**Note:**

>jQuery's data() method will give you access to data-* attributes, BUT, it clobbers the case of the attribute name. [Reference][3] 


  [1]: https://api.jquery.com/attr/
  [2]: https://api.jquery.com/data/
  [3]: http://stackoverflow.com/questions/17351282/jquery-cant-get-data-attribute-value

## Setting value of HTML attribute
If you want to add an attribute to some element you can use the [`attr(attributeName, attributeValue)`][1] function. For example:

    $('a').attr('title', 'Click me');

This example will add mouseover text `"Click me"` to all links on the page.

The same function is used to change attributes' values.


  [1]: https://api.jquery.com/attr/

## Removing attribute
To remove an attribute from an element you can use the function [`.removeAttr(attributeName)`][1]. For example:

    $('#home').removeAttr('title');

This will remove `title` attribute from the element with ID `home`.


  [1]: https://api.jquery.com/removeAttr/

