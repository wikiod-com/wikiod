---
title: "clj-time"
slug: "clj-time"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

This documents deals with how to manipulate Date and Time in clojure.

To use this in your application go to your project.clj file and include 
[clj-time "<version_number>"] in your :dependencies section.

## Comparing two date-time
    (require '[clj-time.core :as t])

    (def date1 (t/date-time 2016 12 5))
    (def date2 (t/date-time 2016 12 6))

    (t/equal? date1 date2) ;; false
    (t/equal? date1 date1) ;; true

    (t/before? date1 date2) ;; true
    (t/before? date2 date1) ;; false

    (t/after? date1 date2) ;; false
    (t/after? date2 date1) ;; true

## Adding joda date-time from other time types
The clj-time.coerce library can help converting other date-time formats to joda time format (clj-time.core/date-time). The other formats include **Java long** format, **String**, **Date**, **SQL Date**. 

To convert time from other time formats, include the library and use the from-<time-type> function, e.g.

    (require '[clj-time.coerce :as c])
    
    (def string-time "1990-01-29")
    (def epoch-time 633571200)
    (def long-time 633551400)
    
    (c/from-string string-time) ;; #<DateTime 1990-01-29T00:00:00.000Z>
    (c/from-epoch epoch-time) ;; #<DateTime 1990-01-29T00:00:00.000Z>
    (c/from-long 633551400) ;; #<DateTime 1990-01-29T00:00:00.000Z>






## Getting Day Month Year Hour Minute Second from your date-time
    (require '[clj-time.core :as t])
    
    (def example-time (t/date-time 2016 12 5 4 3 27 456))

    (t/year example-time) ;; 2016
    (t/month example-time) ;; 12
    (t/day example-time) ;; 5
    (t/hour example-time) ;; 4
    (t/minute example-time) ;; 3
    (t/second example-time) ;; 27

## Adding date-time to other date-times
cljs-time gives us option to add/subtract date-times to other date-times. The date times added subtracted should be in form of days, months, years, hours etc.

    (require '[clj-time.core :as t])
    
    (def example-date (t/date-time 2016 1 1)) ;; #<DateTime 2016-01-01T00:00:00.000Z>
    
    ;; Addition
    (t/plus example-date (t/months 1))        ;; #<DateTime 2016-02-01T00:00:00.000Z>
    (t/plus example-date (t/years 1))         ;; #<DateTime 2017-01-01T00:00:00.000Z>

    ;; Subtraction
    (t/minus example-date (t/days 1))          ;; #<DateTime 2015-12-31T00:00:00.000Z>
    (t/minus example-date (t/hours 12))        ;; #<DateTime 2015-12-31T12:00:00.000Z>

## Creating a Joda Time
    (clj-time/date-time 2017 1 20)

Gives you a Joda time of 20th Jan 2017 at 00:00:00.

Hours, minutes and seconds can also be specified as

    (clj-time/date-time year month date hour minute second millisecond)

## Checking whether a time is within a time interval
This function tells whether a given time lies within a a given time interval. 

    (require '[clj-time.core :as t])
    
    (def date1 (t/date-time 2016 11 5))
    (def date2 (t/date-time 2016 12 5))
    
    (def test-date1 (t/date-time 2016 12 20))
    (def test-date2 (t/date-time 2016 11 15))
    
    (t/within? (t/interval date1 date2) test-date1) ;; false
    (t/within? (t/interval date1 date2) test-date2) ;; true

The interval function is used is **exclusive**, which means that is does not include the second argument of the function within the interval. As an example:

    (t/within? (t/interval date1 date2) date2) ;; false
    (t/within? (t/interval date1 date2) date1) ;; true

