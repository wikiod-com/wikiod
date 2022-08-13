---
title: "Play with English word with INFLECTOR helper"
slug: "play-with-english-word-with-inflector-helper"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Inflector is a very handy helper to change/convert english word to singular, plural, camel case, humanize etc. The helper also help to check whether a word has plural version or not.

## Load inflector helper
To use the method of inflector helper, first load the helper like all other helper with the following code:

    $this->load->helper('inflector');

## Make a word singular
Function `singular($string)`, convert a plural word to singular. To get perfect result parameter `$string` should be a single word. The function will return `string`.

    echo singular("books"); //prints 'book'

 

## Check a word has plural
`is_countalbe($string)` is use for checking a word has plural form or not. Return type will be `boolean` means if the given word has plural form it will return `true`, otherwise will return `false`. 

    is_countable('book'); // Returns TRUE

## Make a word plural
For getting plural form of any English word the `plural($string)` function is handy. Like `singular($string)`, the function `plural($string)` also return `string` result. 

    echo plural("book"); //prints 'books'

## Camelized the string
Camel Case is the practise of writing compound words or phrases where every word begins with Capital letter, without space between word. The function `camelize($string)` helps to make a string camelized. It converts a string of words separated by spaces or underscores to camel case. 

    echo camelize('Mc donald'); //Prints mcDonald

## Remove / Add delimiter between words
Remove delimiter
================
The function `humanize($words)`, takes multiple words separated by underscores and adds spaces for underscores with capitalized each word.

    echo humanize('mac_donald'); // Prints 'Mac Donald'
The function can also replace any declared separator/delimiter. In this case, delimiter will be second parameter.  

    echo humanize('mac-donald','-'); // Prints 'Mac Donald'

    echo humanize('mac#donald','#'); // Prints 'Mac Donald'

Add Underscore
==============
On the other hand, `underscore($words)` function replace the space between words with underscore(_). 

    echo underscore('Mac Donald'); // Prints 'mac_donald'

 

