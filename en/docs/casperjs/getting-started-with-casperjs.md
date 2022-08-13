---
title: "Getting started with casperjs"
slug: "getting-started-with-casperjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting casperjs set up or installed.

## First  program to getting started
    var casper = require('casper').create();
    casper.start('http://casperjs.org/');
    
    casper.then(function() {
        this.echo('First Page: ' + this.getTitle());
    });
    
    casper.thenOpen('http://phantomjs.org', function() {
        this.echo('Second Page: ' + this.getTitle());
    });
    
    casper.run();

