---
title: "Getting and setting width and height of an element"
slug: "getting-and-setting-width-and-height-of-an-element"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Getting and setting width and height (ignoring border)
Get width and height:

    var width = $('#target-element').width();
    var height = $('#target-element').height();

Set width and height:

    $('#target-element').width(50);
    $('#target-element').height(100);

## Getting and setting innerWidth and innerHeight (ignoring padding and border)
Get width and height:

    var width = $('#target-element').innerWidth();
    var height = $('#target-element').innerHeight();

Set width and height:

    $('#target-element').innerWidth(50);
    $('#target-element').innerHeight(100);

##  Getting and setting outerWidth and outerHeight (including padding and border)
Get width and height (excluding margin):

    var width = $('#target-element').outerWidth();
    var height = $('#target-element').outerHeight();

Get width and height (including margin):

    var width = $('#target-element').outerWidth(true);
    var height = $('#target-element').outerHeight(true);

Set width and height:

    $('#target-element').outerWidth(50);
    $('#target-element').outerHeight(100);

