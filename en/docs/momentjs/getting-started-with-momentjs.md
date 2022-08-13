---
title: "Getting started with momentjs"
slug: "getting-started-with-momentjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## General usage
The way `Moment` uses dates and times is by wrapping the existing `Date()` object in a `moment()` object and specifying useful and intuitive methods on this object.

# Format Dates

    moment().format('MMMM Do YYYY, h:mm:ss a'); // August 4th 2016, 10:41:45 am
    moment().format('dddd');                    // Thursday
    moment().format("MMM Do YY");               // Aug 4th 16
    moment().format('YYYY [escaped] YYYY');     // 2016 escaped 2016
    moment().format();                          // 2016-08-04T10:41:45+02:00

# Relative Time

    moment("20111031", "YYYYMMDD").fromNow(); // 5 years ago
    moment("20120620", "YYYYMMDD").fromNow(); // 4 years ago
    moment().startOf('day').fromNow();        // 11 hours ago
    moment().endOf('day').fromNow();          // in 13 hours
    moment().startOf('hour').fromNow();       // 42 minutes ago

# Calendar Time

    moment().subtract(10, 'days').calendar(); // 07/25/2016
    moment().subtract(6, 'days').calendar();  // Last Friday at 10:42 AM
    moment().subtract(3, 'days').calendar();  // Last Monday at 10:42 AM
    moment().subtract(1, 'days').calendar();  // Yesterday at 10:42 AM
    moment().calendar();                      // Today at 10:42 AM
    moment().endOf('day');                    // Today at 23:59 AM
    moment().add(1, 'days').calendar();       // Tomorrow at 10:42 AM
    moment().add(3, 'days').calendar();       // Sunday at 10:42 AM
    moment().add(10, 'days').calendar();      // 08/14/2016

# Multiple Locale Support

    moment.locale();         // en
    moment().format('LT');   // 10:43 AM
    moment().format('LTS');  // 10:43:14 AM
    moment().format('L');    // 08/04/2016
    moment().format('l');    // 8/4/2016
    moment().format('LL');   // August 4, 2016
    moment().format('ll');   // Aug 4, 2016
    moment().format('LLL');  // August 4, 2016 10:43 AM
    moment().format('lll');  // Aug 4, 2016 10:43 AM
    moment().format('LLLL'); // Thursday, August 4, 2016 10:43 AM
    moment().format('llll'); // Thu, Aug 4, 2016 10:43 AM

## Turn a Date object into and from a moment object
To turn a Javascript Date object into a moment (moment object) just call `moment` and pass the Date as a argument

    moment(new Date());
    // Moment {_isAMomentObject: true, _i: Sun Jul 31 2016 11:08:02 GMT+0300 (Jerusalem Daylight Time), _isUTC: false, _pf: Object, _locale: Locale…}

To turn the moment back to a Javascript Date use the `.toDate()` method
````
var now = new Date();
var mom = moment(now)
//undefined
mom
// Moment {_isAMomentObject: true, _i: Sun Jul 31 2016 11:10:32 GMT+0300 (Jerusalem Daylight Time), _isUTC: false, _pf: Object, _locale: Locale…}
mom.toDate()
// Sun Jul 31 2016 11:10:32 GMT+0300 (Jerusalem Daylight Time)
````

## Calculating and manipulating dates
If you have a moment object you can use `add` and `substract` to manipulate it or set any property of the time directly

     moment("2016-01-01").add(1, 'year').format('YYYY-MM-DD')
     // -> "2017-01-01"

Or use `.day()`, `.month()`, `.year()`, `.seconds()`, `.milliseconds()` to set those values directly. Note that the arguments passed are 0-indexed.

     moment("2016-01-01").month(4).day(5).format('YYYY-MM-DD')
    // -> "2016-05-06"

## Date parsing
You can use moment to parse date strings.

By default moment tries to parse the date as an [ISO-8601](https://en.wikipedia.org/wiki/ISO_8601) string and if that does not work falls back to the browsers `new Date()`. Since the way that browsers construct dates varies it is best to try not to fall back to this.

    moment('2016-02-04').format('YYYY-MM-DD')
    // ->'2016-02-04'

If you have a format other then ISO 8601 it is best to pass the format string as a second argument to moment.

    moment('04-02-2016', 'MM-DD-YYYY').format('YYYY-MM-DD')
    // ->'2016-02-04'

## Installation or Setup
Moment is a javascript library that was designed to make working with dates and times less time-consuming and more intuitive.  
You can easily start using this library by installing it in you web-app by using one of the following guides.

# Browser

You can either download the JS file [from the official website][1] or use [cdnjs.com][2].

## With local JS file

    <script src="moment.js"></script>
    <script>
        moment().format();
    </script>

## With CDN

    <script src="https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.14.1/moment.min.js"></script>
    <script>
        moment().format();
    </script>

# Node.js

    npm install moment


Moment should be included inside the `node_modules` directory, Then add it to your script

    var moment = require('moment');
    moment().format();


# Bower

    bower install --save moment

To test if you site or web application is properly using the moment.js script, open console (F12 on the keyboard) and type `moment`, the console should print the moment function if it was included.


  [1]: http://momentjs.com/downloads/moment.js
  [2]: https://cdnjs.com/libraries/moment.js

