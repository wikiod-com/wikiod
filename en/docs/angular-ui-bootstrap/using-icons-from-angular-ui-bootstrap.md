---
title: "Using Icons From Angular UI Bootstrap"
slug: "using-icons-from-angular-ui-bootstrap"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

The Angular UI Bootstrap gives you access to all the standard bootstrap icons in your AngularJS application. In the world of bootstrap these icons are normally referred to as ***glyphicons***. Using these glyphicons wisely can quickly give your app a more polished look and can be a great way to dip your toe into the angular-ui-bootstrap library.  

You can find [a list of all available glyphicons here][1].


  [1]: http://getbootstrap.com/components/

## Installation
You can also follow the official [installation guide here][1].

  [1]: https://github.com/angular-ui/bootstrap

**Step 1) Get The Angular UI Bootstrap Library Files**

via npm:

    npm install angular-ui-bootstrap

via bower:

    bower install angular-bootstrap

**Step 2) Import The Angular UI Bootstrap Module**

    angular.module('myModule', ['ui.bootstrap']);

**Step 3) Use Some Icons!**

You can then add an icon anywhere by creating an <i> tag (yes, the "i" is short for icon) and giving it two classes: one that is always just "glyphicon" and one for the specific glyphicon you want to use. Here is an example:

    <i class="glyphicon glyphicon-fire"></i>



