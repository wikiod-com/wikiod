---
title: "Compass CSS3 Mixins"
slug: "compass-css3-mixins"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Getting started guide using Sass exentsion Compass. Compass is very useful when dealing with CSS3 as it provides mixins to write 1 line in order to support every browser using CSS3 features. It is also great to include sprite images.

## Set up environment
Open your command line

Installation using Ruby
-----------------------
    gem update --system

    gem install compass


----------


Create a Project
----------------
    compass create <myproject>
This will initialize a compass project. It will add a folder called <myproject>. The folder will look like have the following structure:

| File/Folder | description |
| ------ | ------ |
| sass/ | Put you sass/scss files in this folder |
| stylesheets/ | In this folder your compiled css will be stored |
| config.rb | Configure compass - e.g. folder path, sass compilation |


----------

Use compass
-----------

    compass watch

This will compile your sass files every time you change them. The sass folder path can be changed inside of the config.rb


## Using CSS3 with compass
You can find a complete reference which CSS3 components are supported on this [page][1]

In order to use CSS3 in your project Compass provides mixins to support CSS3 features in every browser. On top of your Sass/Scss file you have to specify that you want to use compass

    @import "compass/css3";


----------

Border-radius
-------------
Include border-radius with compass in your sass file:

    div {
        @include border-radius(4px);
    }

CSS output

    div {
      -moz-border-radius: 4px;
      -webkit-border-radius: 4px;
      border-radius: 4px;
    }
As you can see you can use the normal CSS name. Just put @include in front of it and use ( ) to set your value.


----------

Flexbox Example
-------

    .row {
      @include display-flex;
      @include flex-direction(row);
    }

CSS Output

    .row {
      display: -webkit-flex;
      display: flex;
      -webkit-flex-direction: row;
      flex-direction: row;
    }


----------


Conclusion
----------

This are only two examples. Compass provides much more CSS3 mixins. It is very handy to use Compass and you don't have to worry that you have forgot defining a CSS3 component for a specified browser. If the browser supports the CSS3 feature, compass will define it for you.

  [1]: http://compass-style.org/reference/compass/css3/

