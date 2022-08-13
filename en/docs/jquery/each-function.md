---
title: "Each function"
slug: "each-function"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## jQuery each function
HTML:

    <ul>
      <li>Mango</li>
      <li>Book</li>
    </ul>
Script:

    $( "li" ).each(function( index ) {
      console.log( index + ": " + $( this ).text() );
    });
A message is thus logged for each item in the list:

0: Mango 

1: Book



## Basic use
    // array
    var arr = [
       'one',
       'two',
       'three',
       'four'
    ];
    $.each(arr, function (index, value) {
      console.log(value);
      
      // Will stop running after "three"
      return (value !== 'three');
    });
    // Outputs: one two three

