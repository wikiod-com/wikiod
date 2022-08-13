---
title: "Carousels"
slug: "carousels"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

For more information, visit the official documentation at http://getbootstrap.com/javascript/#carousel, where the basic HTML and Javascript usage examples and information are derived from.

It should be noted that carousels do not function correctly in IE 9 and earlier due to the use of CSS3 transitions/animations.



## Basic HTML usage
A Bootstrap carousel is a Bootstrap component that creates a slideshow which cycles through elements within the carousel.

Here is a basic HTML usage example:

    <div id="carousel-example-generic" class="carousel slide" data-ride="carousel">
      <!-- Indicators -->
      <ol class="carousel-indicators">
        <li data-target="#carousel-example-generic" data-slide-to="0" class="active"></li>
        <li data-target="#carousel-example-generic" data-slide-to="1"></li>
        <li data-target="#carousel-example-generic" data-slide-to="2"></li>
      </ol>
    
      <!-- Wrapper for slides -->
      <div class="carousel-inner" role="listbox">
        <div class="item active">
          <img src="..." alt="...">
          <div class="carousel-caption">
            ...
          </div>
        </div>
        <div class="item">
          <img src="..." alt="...">
          <div class="carousel-caption">
            ...
          </div>
        </div>
        ...
      </div>
    
      <!-- Controls -->
      <a class="left carousel-control" href="#carousel-example-generic" role="button" data-slide="prev">
        <span class="glyphicon glyphicon-chevron-left" aria-hidden="true"></span>
        <span class="sr-only">Previous</span>
      </a>
      <a class="right carousel-control" href="#carousel-example-generic" role="button" data-slide="next">
        <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>
        <span class="sr-only">Next</span>
      </a>
    </div>

## Basic Javascript usage and initialization
Carousel components can be instantiated via jQuery with the function `$('.carousel').carousel(options)`, where `$('.carousel')` is a top-level reference to the specific carousel and `options` is a Javascript object specifying the carousel's default attributes.
 
The `options` object allows for multiple properties to be defined which will affect how the carousel behaves. These properties are defined as such:
 
 - The `interval` property accepts a Javascript `number` type which allows a user to define the amount of time the carousel displays a given carousel slide for. If the boolean value `false` is specified, the carousel will not cycle automatically.
 - The `pause` property accepts a Javascript `string` type which toggles behavior where the carousel's automatic cycling is paused when the user's mouse enters the carousel.  The default (and only) value accepted is "hover". 
 - The `wrap` property accepts a Javascript `boolean` type which allows a user to define whether or not they want the carousel to continuously cycle without stopping on a given slide.
 - The `keyboard` property accepts a Javascript `boolean` type which allows a user to define whether or not they want the carousel to respond to keyboard events.
 
Here is an example of the basic Javascript usage:
 
    $('#carCarousel').carousel({ interval: 2500, pause: "hover", wrap: false, keyboard: true });
 
As with other Bootstrap components, the carousel's options can also be specified in HTML via data attributes.

