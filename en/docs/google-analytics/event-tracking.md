---
title: "Event Tracking"
slug: "event-tracking"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Below is the example of how to hard code the google analytics implementation in the website.
To track the shopping cart actions let induce the tracking snippet. It look as below.
You can track the events in google analytics open source tool

## Syntax
 - `ga('send', 'event', [eventCategory], [eventAction], [eventLabel], [eventValue], [fieldsObject]);`

## Parameters
| Field Name | Description | 
| ------ | ------ |
| eventCategory   | Typically the object that was interacted with (e.g. 'Video')   |
| eventAction   | The type of interaction (e.g. 'play')   |
| eventLabel   | Useful for categorizing events (e.g. 'Fall Campaign')   |
| eventValue   | A numeric value associated with the event (e.g. 42)   |

## Tracking searches within your site
Insert the following function call within your JavaScript when searching within your website to track how visitors are using your internal search features.

In this example, the Event Action `filters` is a comma-delimited list of search filter name/value pairs, and the Event Label `orderedBy` is a string describing the user-determined sort order.

    ga('send', 'event', 'Product Search', filters, orderedBy);

These events can then be viewed within Analytics under Behavior > Events:

[![Event shown by Action][1]][1]

Likewise, the searches can be viewed by Label to see how users are sorting their data:
[![Event shown by Label][2]][2]


  [1]: http://i.stack.imgur.com/9Xbi8.png
  [2]: http://i.stack.imgur.com/3OusO.png

## Track shopping cart actions
Adding a product to a shopping cart (Label `item.name` references the name property of the product added):

    ga('send', 'event', 'Cart', 'Add', product.name);

This lets you see what people are adding to the shopping cart, even if they never complete the order, allowing more insight into where users are abandoning their session:
[![A list of which products were added to the custom shopping cart][1]][1]

Removing an item from a shopping cart:

    ga('send', 'event', 'Shopping', 'Removed', product.name);

Emptying a shopping cart:

    ga('send', 'event', 'Cart', 'Emptied', 'empty');


  [1]: http://i.stack.imgur.com/cRAjD.png

