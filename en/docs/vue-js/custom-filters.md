---
title: "Custom Filters"
slug: "custom-filters"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Syntax
- `Vue.filter(name, function(value){});` //Basic
- `Vue.filter(name, function(value, begin, end){});` //Basic with wrapping values
- `Vue.filter(name, function(value, input){});` //Dynamic
- `Vue.filter(name, { read: function(value){}, write: function(value){} });` //Two-way

## Parameters
| Parameter | Details |
| ------ | ------ |
| name   | String - desired callable name of the filter   |
| value  | [Callback] Any - value of the data passing into the filter |
| begin  | [Callback] Any - value to come before the passed data |
| end    | [Callback] Any - value to come after the passed data |
| input  | [Callback] Any - user input bound to Vue instance for dynamic results | 

## Basic
Custom filters in `Vue.js` can be created easily in a single function call to `Vue.filter`.

    //JS
    Vue.filter('reverse', function(value) {
        return value.split('').reverse().join('');
    });
    
    //HTML
    <span>{{ msg | reverse }}</span> //'This is fun!' => '!nuf si sihT'

It is good practice to store all custom filters in separate files e.g. under `./filters` as it is then easy to re-use your code in your next application.
If you go this way you have to **replace JS part**:

    //JS
    Vue.filter('reverse', require('./filters/reverse'));
    

You can also define your own `begin` and `end` wrappers as well.

    //JS
    Vue.filter('wrap', function(value, begin, end) {
        return begin + value + end;
    });
    
    //HTML
    <span>{{ msg | wrap 'The' 'fox' }}</span> //'quick brown' => 'The quick brown fox'

## Two-way Filters
With a `two-way filter`, we are able to assign a `read` *and* `write` operation for a single `filter` that changes the value of the *same* data between the `view` and `model`.

    //JS
    Vue.filter('uppercase', {
        //read : model -> view
        read: function(value) {
            return value.toUpperCase();
        },
    
        //write : view -> model
        write: function(value) {
            return value.toLowerCase();
        }
    });
    
    /*
     * Base value of data: 'example string'
     *
     * In the view : 'EXAMPLE STRING'
     * In the model : 'example string'
     */

