---
title: "Different DateTime format in shell"
slug: "different-datetime-format-in-shell"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Parameters
| Format | Interpreted as |
| ------ | -------------- |
`%%` | literal percent sign (%)
`%A` | weekday name (e.g. Sunday)
`%a` | weekday name in short fomat (e.g. Sun)
`%B` | full month name (e.g. January)
`%b` | month name (e.g. Jan)
`%H` | hour (00..23)
`%I` | hour (01..12)
`%j` | day of year (001..366)
`%k` | hour ( 0..23)
`%l` | hour ( 1..12)
`%M` | minute (00..59)
`%m` | month (01..12)
`%p` | Define AM or PM; blank if not known
`%R` | 24-hour hour and minute; same as %H:%M
`%r` | 12-hour clock time (e.g. 11:11:04 PM)
`%S` | second (00..60)
`%s` | Unix epoch: seconds since 1970-01-01 00:00:00 UTC _(not available in older UNIXes)_
`%T` | time, equivalent to `%H:%M:%S`
`%Z` | time zone name (e.g. PDT)
`%z` | time zone offset (direction, hours, minutes, e.g. -0700)


Below are a few useful links for the `date` command in Unix shells:

* Linux: [GNU man page for date](http://man7.org/linux/man-pages/man3/strftime.3.html), includes format codes, see also [GNU man page for strftime](http://man7.org/linux/man-pages/man3/strftime.3.html)
* FreeBSD: [BSD man page for date](http://www.unix.com/man-page/FreeBSD/1/date/), format codes live in [BSD man page for strftime](http://www.unix.com/man-page/FreeBSD/3/strftime/)
* Apple: [OS X man page for date](https://developer.apple.com/library/Mac/documentation/Darwin/Reference/ManPages/man1/date.1.html), format codes live in [OS X man page for strftime](https://developer.apple.com/library/Mac/documentation/Darwin/Reference/ManPages/man3/strftime.3.html)
* Epoch: [Unix time](https://en.wikipedia.org/wiki/Unix_time), also known as POSIX time (`%s`, seconds since 1970)

## Sample Code & output
    #!/bin/bash

    #Print Date / Time in different Formats
    date1=$(date +'%d-%m-%y')
    date2=$(date +'%d-%m-%Y')
    date3=$(date +'%d-%b-%Y')
    date4=$(date +'%d-%B-%Y')
    date5=$(date +'%a %d-%b-%Y')
    date6=$(date +'%a %d-%b-%Y  %Z')
    date7=$(date +'%A %d-%b-%Y')

    echo "Print Date in different format"
    echo $date1
    echo $date2
    echo $date3
    echo $date4
    echo $date5
    echo $date6
    echo $date7
    echo

    #print Timestamp
    time1=$(date '+%H:%M:%S')
    time2=$(date '+%I:%M:%S')
    time3=$(date '+%r')
    time4=$(date '+%R')

    echo "Print Time in different format"
    echo "Time in 24h clock: $time1"
    echo "Time in 12h clock: $time2"
    echo "Time with AM/PM: $time3"
    echo "Time in hour&minute: $time4"

    exit

# Output

<!-- language: none -->

    Print Date in different format
    01-08-16
    01-08-2016
    01-Aug-2016
    01-August-2016
    Mon 01-Aug-2016
    Mon 01-Aug-2016 IST
    Monday 01-Aug-2016

    Print Time in different format
    Time in 24h clock: 15:16:06
    Time in 12h clock: 03:16:06
    Time with AM/PM: 03:16:06 PM
    Time in hour&minute: 15:16

