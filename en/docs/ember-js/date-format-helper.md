---
title: "Date Format Helper"
slug: "date-format-helper"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Helper for a clean date and hour format.
When you want the current date and time, you can do this with the Javascript function `Date`, but will return the following format which isn't always useful: `Wed Jun 07 2017 13:26:15 GMT+0200 (Romance (zomertijd))`.

Copy the following code into `app/helpers/helpers.js`, and simply call `getCurrentDateAndFormat()` instead of `new Date()`.


    export function getCurrentDateAndFormat() {
        let today = new Date();
        let dd = today.getDate();
        let MM = today.getMonth()+1; //January is 0!
        let hh = today.getHours();
        let mm = today.getMinutes();
        let yyyy = today.getFullYear();

        if (dd<10) {
            dd= '0'+dd;
        }

        if (MM<10) {
            MM= '0'+MM;
        }

        if (hh<10) {
            hh= '0'+hh;
        }

        if (mm<10) {
            mm= '0'+mm;
        }

        today = dd+'/'+MM+'/'+yyyy+" "+hh+"h"+mm;

        return today;
    }

The helper extracts all separate time values, adds a 0 if the value is below 10 (for format and readability) and reassembles them in a more fitting format. In this case: day, month, year, hours and minutes.

    today = dd+'/'+MM+'/'+yyyy+" "+hh+"h"+mm;
will display: `07/06/2017 13h26`

    today = MM+'/'+dd+'/'+yyyy+" "+hh+"h"+mm;
will display: `06/07/2017 13h26`

Changing month and date position, depending on your region, is as easy as replacing `MM` with `dd` and vice versa, as evident from above example.


