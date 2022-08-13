---
title: "Date-time classes (POSIXct and POSIXlt)"
slug: "date-time-classes-posixct-and-posixlt"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

R includes two date-time classes -- POSIXct and POSIXlt -- see `?DateTimeClasses`. 

# Pitfalls

With POSIXct, midnight will display only the date and time zone, though the full time is still stored.

# Related topics

 - https://www.wikiod.com/r/date-and-time

# Specialized packages

 - lubridate

## Formatting and printing date-time objects
```r
# test date-time object
options(digits.secs = 3)
d = as.POSIXct("2016-08-30 14:18:30.58", tz = "UTC")   

format(d,"%S")  # 00-61 Second as integer
## [1] "30"

format(d,"%OS") # 00-60.99… Second as fractional
## [1] "30.579"

format(d,"%M")  # 00-59 Minute
## [1] "18"

format(d,"%H")  # 00-23 Hours
## [1] "14"

format(d,"%I")  # 01-12 Hours
## [1] "02"

format(d,"%p")  # AM/PM Indicator
## [1] "PM"

format(d,"%z")  # Signed offset
## [1] "+0000"

format(d,"%Z")  # Time Zone Abbreviation
## [1] "UTC"
```

See `?strptime` for details on the format strings here, as well as other formats.


## Date-time arithmetic
To add/subtract time, use POSIXct, since it stores times in seconds
    
    ## adding/subtracting times - 60 seconds
    as.POSIXct("2016-01-01") + 60
    # [1] "2016-01-01 00:01:00 AEDT"
    
    ## adding 3 hours, 14 minutes, 15 seconds
    as.POSIXct("2016-01-01") + ( (3 * 60 * 60) + (14 * 60) + 15)
    # [1] "2016-01-01 03:14:15 AEDT"

More formally, `as.difftime` can be used to specify time periods to add to a date or datetime object. E.g.:

    as.POSIXct("2016-01-01")         + 
      as.difftime(3,  units="hours") +
      as.difftime(14, units="mins")  +
      as.difftime(15, units="secs")
    # [1] "2016-01-01 03:14:15 AEDT"


To find the difference between dates/times use `difftime()` for differences in seconds, minutes, hours, days or weeks.

    # using POSIXct objects
    difftime(
      as.POSIXct("2016-01-01 12:00:00"), 
      as.POSIXct("2016-01-01 11:59:59"), 
      unit = "secs")
    # Time difference of 1 secs

To generate sequences of date-times use `seq.POSIXt()` or simply `seq`.
    

## Parsing strings into date-time objects
The functions for parsing a string into POSIXct and POSIXlt take similar parameters and return a similar-looking result, but there are differences in how that date-time is stored; see "Remarks."

    as.POSIXct("11:38",                        # time string
               format = "%H:%M")               # formatting string
    ## [1] "2016-07-21 11:38:00 CDT"           
    strptime("11:38",                          # identical, but makes a POSIXlt object
             format = "%H:%M")
    ## [1] "2016-07-21 11:38:00 CDT"
    
    as.POSIXct("11 AM",                   
               format = "%I %p")        
    ## [1] "2016-07-21 11:00:00 CDT"
    
Note that date and timezone are imputed. 

    
    as.POSIXct("11:38:22",                 # time string without timezone
               format = "%H:%M:%S",   
               tz = "America/New_York")    # set time zone
    ## [1] "2016-07-21 11:38:22 EDT"
    
    as.POSIXct("2016-07-21 00:00:00",
               format = "%F %T")           # shortcut tokens for "%Y-%m-%d" and "%H:%M:%S"

See `?strptime` for details on the format strings here.

--------

# Notes

## Missing elements 
- If a date element is not supplied, then that from the current date is used. 
- If a time element is not supplied, then that from midnight is used, i.e. 0s.
- If no timezone is supplied in either the string or the `tz` parameter, the local timezone is used.


## Time zones
- The accepted values of `tz` depend on the location.
    - `CST` is given with `"CST6CDT"` or `"America/Chicago"`
- For supported locations and time zones use:
    - In R: `OlsonNames()` 
    - Alternatively, try in R: `system("cat $R_HOME/share/zoneinfo/zone.tab")`
- These locations are given by [Internet Assigned Numbers Authority (IANA)](http://www.iana.org/time-zones) 
    - [List of tz database time zones (Wikipedia)](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones)
    - [IANA TZ Data (2016e)](http://www.iana.org/time-zones/repository/releases/tzdata2016e.tar.gz) 





