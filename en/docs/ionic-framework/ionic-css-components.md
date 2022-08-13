---
title: "Ionic CSS components"
slug: "ionic-css-components"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

Ionic has a lot of great ready declared CSS components to make your life easier while coding your next hybrid mobile application. These components vary from a basic grid system to styling your forms. These components are in your use if you choose to install Ionic with the pre-set CSS stylesheets.

One of the basic functions that Ionic CSS brings to your hand is that it comes with a set of colors to start with, but as a general rule colors are meant to be overridden. Utility colors are added to help set a naming convention. You could call it a basic theme of the application. To customize the colors you can simply override those coming from the ionic.css CSS file. Additionally, since Ionic is built using Sass, for more power and flexibility you could also modify and extend the color variables within the _variables.scss file.

You can setup an Ionic project to use SASS very easily by running the `ionic setup sass` command in your terminal.

You can find the official documentation on Ionic CSS here: http://ionicframework.com/docs/components/

## Basic grid system syntax
Basic grid
==========

In Ionic you can declare rows by setting the `row` class to a element. Rows will be elements which are horizontally aligned and anything inside this element everything will belong to the row. Inside the row you can declare different width columns. You have a choice of the following declarations.

| Class | Width |
| ------ | ------ |
|.col-10 |   10% |
|.col-20 |   20% |
|.col-25 |   25% |
|.col-33 |   33.3333% |
|.col-50 |   50% |
|.col-67 |   66.6666% |
|.col-75 |   75% |
|.col-80 |   80% |
|.col-90 |   90% |

The maximum value columns may have inside a row is 100. Here's a few examples of the basic grid.

    <div class="row">
      <div class="col col-50">.col.col-50</div>
      <div class="col">.col</div>
      <div class="col">.col</div>
    </div>

    <div class="row">
      <div class="col col-75">.col.col-75</div>
      <div class="col">.col</div>
    </div>

    <div class="row">
      <div class="col">.col</div>
      <div class="col col-75">.col.col-75</div>
    </div>

    <div class="row">
      <div class="col">.col</div>
      <div class="col">.col</div>
    </div>

Offset grids
============

You can also set `col-offset-<value>` to the columns. In the example below the one-third of a width column has one-third of the width offset, which means it will be one-third width wide and be centered in the page because of it's offset.

    <div class="row">
      <div class="col col-33 col-offset-33">.col</div>
      <div class="col">.col</div>
    </div>

Align columns
=============

Aligning columns vertically is possibly by setting the `col-<align_value>` to a column separately like this.

    <div class="row">
      <div class="col col-top">.col</div>
      <div class="col col-center">.col</div>
      <div class="col col-bottom">.col</div>
      <div class="col">1<br>2<br>3<br>4</div>
    </div>

The above will align each column on it's own. If you want to align all the columns inside the row you can set the align value to the row itself. In the example below all the columns inside this row will align themselves vertically in the center of the row.

    <div class="row row-center">
      <div class="col">.col</div>
      <div class="col">.col</div>
      <div class="col">.col</div>
      <div class="col">1<br>2<br>3<br>4</div>
    </div>

Responsive grid
===============

You might also want to have the columns responsive as of they will stack on top of each other at some viewport width. You have three choices.

| Class | Breakpoint (approximately) |
| ------ | ------ |
| .responsive-sm |   Smaller than landscape phone |
| .responsive-md |   Smaller than portrait tablet |
| .responsive-lg |   Smaller than landscape tablet |

In this example these columns will stack under the width of approximately a landscape phone.

    <div class="row responsive-sm">
      <div class="col">.col</div>
      <div class="col">.col</div>
      <div class="col">.col</div>
      <div class="col">.col</div>
    </div>

You can also make your own media queries of course if these breakpoints are not suitable for you and/or you need more specific breakpoints.

## Basic list item syntax
Almost every application has some kind of a list. Ionic has it's own build-in ready-to-go list item CSS declarations to make it easy to do lists inside your application. You can either use HTML elements and declare a class for the or use the directive `ion-list` to make them. Example of a directive is at the bottom.

Basic list item CSS syntax:

    <ul class="list">
      <li class="item"></li>
    </ul>

List with dividers:

    <div class="list">
      <a class="item" href="#">
        List item
      </a>
      <div class="item item-divider">
        Divider that looks a bit different from items
      </div>
      <a class="item" href="#">
        Another list item
      </a>
    </div>

List items with icons:

    <div class="list">
      <a class="item item-icon-left" href="#">
        <i class="icon ion-trash-b"></i>
        List item with a trashcan icon on the left
      </a>
    </div>

You can also set icons on both sides of the items with the following syntax:

    <div class="list">
      <a class="item item-icon-left item-icon-right" href="#">
        <i class="icon ion-trash-b"></i>
        List item with a trashcan icon on the left and a briefcase icon on the right
        <i class="icon ion-briefcase"></i>
      </a>
    </div>

An list item with button or buttons can be created like this:

    <div class="list">
      <div class="item item-button-right">
        Item with a button on the right that has a clock icon in it
        <button class="button button-positive">
          <i class="icon ion-clock"></i>
        </button>
      </div>
    </div>

It's also possible to create list items with avatars, thumbnails and inset which will create padding around the list items. Ionic also handles setting icons etc in list items by setting padding accordingly to the list items.

Ionic also has it's own directives for checkboxes, radio buttons etc. Here's an example of a checkbox list with Ionic.

    <ion-list>
      <ion-checkbox ng-model="choice1">Choice 1</ion-checkbox>
      <ion-checkbox ng-model="choice2">Choice 2</ion-checkbox>
    </ion-list>

## Basic usage of utility colors
Preset Ionic CSS will have a theme and pre-set colors for it. You can modify or override the basic values in the ionic.css or in your custom CSS file. You can also define these with SASS and to use SASS in Ionic you just need to run the `ionic setup sass` command in your terminal.

Basic usage of colors in a button. The `button-<phrase>` class will make the button background and borders the color of the set theme.

    <button class="button button-positive">
      button-positive
    </button>
    
    <button class="button button-calm">
      button-calm
    </button>
    
    <button class="button button-balanced">
      button-balanced
    </button>

Your CSS prefix choices are the following:

  - `<element>-light`

  - `<element>-stable`

  - `<element>-positive`

  - `<element>-calm`

  - `<element>-balanced`

  - `<element>-energized`

  - `<element>-assertive`

  - `<element>-royal`

  - `<element>-dark`

These classes can be added also for example in badges etc. Here's an example of a badge:

    <span class="badge badge-positive">Positive badge</span>

