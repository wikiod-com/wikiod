---
title: "The Date class"
slug: "the-date-class"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

# Related topics

 - https://www.wikiod.com/r/date-and-time

# Jumbled notes

- `Date`: Stores time as number of days since UNIX epoch on `1970-01-01`. with negative values for earlier dates.
- It is represented as an integer (however, it is not enforced in the internal representation)
- They are always printed following the rules of the current Gregorian calendar, even though the calendar was not in use a long time ago. 
- It doesn't keep track of timezones, so it should not be used to truncate the time out of `POSIXct` or `POSIXlt` objects.
- `sys.Date()` returns an object of class `Date`

# More notes
- [lubridate](https://github.com/hadley/lubridate)'s `ymd`, `mdy`, etc. are alternatives to `as.Date` that also parse to Date class; see [Parsing dates and datetimes from strings with lubridate](https://www.wikiod.com/r/date-and-time/date-and-time/7018/parsing-dates-and-datetimes-from-strings-with-lubridate).
- [data.table](https://github.com/Rdatatable/data.table/wiki)'s experimental IDate class is derived from and is mostly interchangeable with Date, but is stored as integer instead of double.

## Formatting Dates
To format `Dates` we use the `format(date, format="%Y-%m-%d")` function with either the `POSIXct` (given from `as.POSIXct()`) or `POSIXlt` (given from `as.POSIXlt()`)

```r
d = as.Date("2016-07-21") # Current Date Time Stamp

format(d,"%a")            # Abbreviated Weekday
## [1] "Thu"

format(d,"%A")            # Full Weekday
## [1] "Thursday"

format(d,"%b")            # Abbreviated Month
## [1] "Jul"

format(d,"%B")            # Full Month
## [1] "July"

format(d,"%m")            # 00-12 Month Format
## [1] "07"

format(d,"%d")            # 00-31 Day Format
## [1] "21"

format(d,"%e")            # 0-31 Day Format
## [1] "21"

format(d,"%y")            # 00-99 Year
## [1] "16"

format(d,"%Y")            # Year with Century
## [1] "2016"
```

For more, see `?strptime`.

## Parsing Strings into Date Objects
R contains a Date class, which is created with `as.Date()`, which takes a string or vector of strings, and if the date is not in ISO 8601 date format `YYYY-MM-DD`, a formatting string of `strptime`-style tokens.

    as.Date('2016-08-01')    # in ISO format, so does not require formatting string
    ## [1] "2016-08-01"

    as.Date('05/23/16', format = '%m/%d/%y')
    ## [1] "2016-05-23"

    as.Date('March 23rd, 2016', '%B %drd, %Y')    # add separators and literals to format
    ## [1] "2016-03-23"

    as.Date('  2016-08-01  foo')    # leading whitespace and all trailing characters are ignored
    ## [1] "2016-08-01"

    as.Date(c('2016-01-01', '2016-01-02'))
    # [1] "2016-01-01" "2016-01-02"



## Dates
To coerce a variable to a date use the `as.Date()` function. 

    > x <- as.Date("2016-8-23")
    > x
    [1] "2016-08-23"
    > class(x)
    [1] "Date"

The `as.Date()` function allows you to provide a format argument. The default is `%Y-%m-%d`, which is Year-month-day.

    > as.Date("23-8-2016", format="%d-%m-%Y") # To read in an European-style date
    [1] "2016-08-23"

The format string can be placed either within a pair of single quotes or double quotes.  Dates are usually expressed in a variety of forms such as: `"d-m-yy"` or `"d-m-YYYY"` or `"m-d-yy"` or `"m-d-YYYY"` or `"YYYY-m-d"` or `"YYYY-d-m"`. These formats can also be expressed by replacing `"-"` by `"/"`. Furher, dates are also expressed in the forms, say, "Nov 6, 1986" or "November 6, 1986" or "6 Nov, 1986" or "6 November, 1986" and so on. The **as.Date()** function accepts all such character strings and when we mention the appropriate format of the string, it always outputs the date in the form `"YYYY-m-d"`.

Suppose we have a date string `"9-6-1962"` in the format `"%d-%m-%Y"`.

    #
    # It tries to interprets the string as YYYY-m-d
    #
    > as.Date("9-6-1962")
    [1] "0009-06-19"       #interprets as "%Y-%m-%d"
    > 
    as.Date("9/6/1962")
    [1] "0009-06-19"       #again interprets as "%Y-%m-%d"
    >
    # It has no problem in understanding, if the date is in form  YYYY-m-d or YYYY/m/d
    #
    > as.Date("1962-6-9")
    [1] "1962-06-09"        # no problem
    > as.Date("1962/6/9")
    [1] "1962-06-09"        # no problem
    > 
    
By specifying the correct format of the input string, we can get the desired results. We use the following codes for specifying the formats to the **as.Date()** function.



|Format Code | Meaning |
| ------ | ------ |
| `%d`   | day   |
| `%m`   | month|
| `%y`   | year in 2-digits|
| `%Y`   | year in 4-digits|
| `%b`   | abbreviated month in 3 chars|
| `%B`   | full name of the month|


Consider the following example specifying the **format** parameter:

    
    > as.Date("9-6-1962",format="%d-%m-%Y")
    [1] "1962-06-09"
    >

The parameter name **format** can be omitted.

    > as.Date("9-6-1962", "%d-%m-%Y")
    [1] "1962-06-09"
    >

Some times, names of the months abbreviated to the first three characters are used in the writing the dates. In which case we use the format specifier `%b`.

    > as.Date("6Nov1962","%d%b%Y")
    [1] "1962-11-06"
    >

Note that, there are no either `'-'` or `'/'` or white spaces between the members in the date string. The format string should exactly match that input string. Consider the following example:

    > as.Date("6 Nov, 1962","%d %b, %Y")
    [1] "1962-11-06"
    >

 
Note that, there is a comma in the date string and hence a comma in the format specification too. If comma is omitted in the format string, it results in an `NA`. An example usage of `%B` format specifier is as follows:

    > as.Date("October 12, 2016", "%B %d, %Y")
    [1] "2016-10-12"
    >
    > as.Date("12 October, 2016", "%d %B, %Y")
    [1] "2016-10-12"
    > 


`%y` format is system specific and hence, should be used with caution. Other parameters used with this function are **origin** and **tz**( time zone).





