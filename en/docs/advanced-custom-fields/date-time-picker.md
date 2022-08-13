---
title: "Date Time Picker"
slug: "date-time-picker"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The Date Time Picker allows a user to choose both a date (month/date/year) as well as a time of day (hours/minutes/seconds). A user can then output the date format and return format based on [PHP date()][1] acceptable methods. This is helpful for querying custom post types like events, tour dates, or even a single custom field.


  [1]: http://php.net/manual/en/function.date.php

## Ouput date-time value
Display the date by itself (custom field is `tour_date`):
    
    <p>Tour Date <?php the_field('tour_date');?></p>

## Use two date-time fields to show a field
Let's say you want to show a text field (`announcement`) based on the time (a scheduled notification message for instance).

You need two date-time fields. In our example, one is called `start_date` and the other `end_date`.

    <?php 
    $DateNow = date('Y-m-d H:i:s');
    $DateStart = get_field('start_date', false, false);
    $DateEnd = get_field('end_date', false, false);
    
    if($DateNow > $DateStart && $DateNow < $DateEnd) {
       echo the_field('announcement');
    }    
    ?>

