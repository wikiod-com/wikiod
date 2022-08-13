---
title: "Credits"
slug: "credits"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Removing "highcharts.com" Logo
Highchart by default puts a credits label in the *lower right corner* of the chart. 

[![HighCharts Credits Logo][1]][1]

This can be removed using `credits` option in your chart settings.

 

    credits: {
        enabled: false
    }

Or

    credits: false

will remove the [highcharts.com](http://www.highcharts.com) logo.


  [1]: http://i.stack.imgur.com/PX89k.jpg

## style: CSSObject
CSS styles for the credits label. Defaults to:

    credits: {
        style: {
            cursor: 'pointer',
            color: '#909090',
            fontSize: '10px'
        }
    },

## position: Object
Position configuration for the credits label. Supported properties are `align`, `verticalAlign`, `x` and `y`. 

Defaults to:

    credits: {
        position: {
            align: 'right',
            x: -10,
            verticalAlign: 'bottom',
            y: -5
        }
    },

## text: String and href: String
The URL for the credits label. 

Defaults to: http://www.highcharts.com.

    credits: {
        text: 'StackOverflow.com',
        href: 'http://stackoverflow.com'
    },

