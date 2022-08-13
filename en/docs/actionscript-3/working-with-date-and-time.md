---
title: "Working with Date and Time"
slug: "working-with-date-and-time"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Ante meridiem (AM) or Post meridiem (PM) for 12-hour clock
    function meridiem(d:Date):String {
        return (d.hours > 11) ? "pm" : "am";
    }

## Number of days in the specified month
    /**
     * @param year   Full year as int (ex: 2000).
     * @param month  Month as int, zero-based (ex: 0=January, 11=December).
     */
    function daysInMonth(year:int, month:int):int {
        return (new Date(year, ++month, 0)).date;
    }

## Whether the specified year is leap year
    function isLeapYear(year:int):Boolean {
        return daysInMonth(year, 1) == 29;
    }

## Whether daylight savings time is currently observed
    function isDaylightSavings(d:Date):Boolean {
        var months:uint = 12;
        var offset:uint = d.timezoneOffset;
        var offsetCheck:Number;
    
        while (months--) {
            offsetCheck = (new Date(d.getFullYear(), months, 1)).timezoneOffset;
    
            if (offsetCheck != offset)
                return (offsetCheck > offset);
        }
    
        return false;
    }

## Today's start date, at midnight
    function today(date:Date = null):Date {
        if (date == null)
            date = new Date();
    
        return new Date(date.fullYear, date.month, date.date, 0, 0, 0, 0);
    }

