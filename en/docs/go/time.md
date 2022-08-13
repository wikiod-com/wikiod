---
title: "Time"
slug: "time"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

The Go [`time`](https://golang.org/pkg/time/) package provides functionality for measuring and displaying time.

This package provide a structure `time.Time`, allowing to store and do computations on dates and time.

## Syntax
- time.Date(2016, time.December, 31, 23, 59, 59, 999, time.UTC) // initialize
- date1 == date2 // returns `true` when the 2 are the same moment
- date1 != date2 // returns `true` when the 2 are different moment
- date1.Before(date2) // returns `true` when the first is strictly before the second
- date1.After(date2) // returns `true` when the first is strictly after the second

## Return time.Time Zero Value when function has an Error
```go
const timeFormat = "15 Monday January 2006"

func ParseDate(s string) (time.Time, error) {
    t, err := time.Parse(timeFormat, s)
    if err != nil {
        // time.Time{} returns January 1, year 1, 00:00:00.000000000 UTC
        // which according to the source code is the zero value for time.Time
        // https://golang.org/src/time/time.go#L23
        return time.Time{}, err
    }
    return t, nil
}

## Time parsing
If you have a date stored as a string you will need to parse it. Use `time.Parse`.

    //           time.Parse(   format   , date to parse)
    date, err := time.Parse("01/02/2006",  "04/08/2017")
    if err != nil {
        panic(err)
    }
    
    fmt.Println(date)
    // Prints 2017-04-08 00:00:00 +0000 UTC

The first parameter is the layout in which the string stores the date and the second parameter is the string that contains the date. `01/02/2006` is the same than saying the format is `MM/DD/YYYY`.

The layout defines the format by showing how the reference time, defined to be `Mon Jan 2 15:04:05 -0700 MST 2006` would be interpreted if it were the value; it serves as an example of the input format. The same interpretation will then be made to the input string.

You can see the constants defined in the time package to know how to write the layout string, but note that the constants are not exported and can't be used outside the time package.

    const (
        stdLongMonth             // "January"
        stdMonth                 // "Jan"
        stdNumMonth              // "1"
        stdZeroMonth             // "01"
        stdLongWeekDay           // "Monday"
        stdWeekDay               // "Mon"
        stdDay                   // "2"
        stdUnderDay              // "_2"
        stdZeroDay               // "02"
        stdHour                  // "15"
        stdHour12                // "3"
        stdZeroHour12            // "03"
        stdMinute                // "4"
        stdZeroMinute            // "04"
        stdSecond                // "5"
        stdZeroSecond            // "05"
        stdLongYear              // "2006"
        stdYear                  // "06"
        stdPM                    // "PM"
        stdpm                    // "pm"
        stdTZ                    // "MST"
        stdISO8601TZ             // "Z0700"  // prints Z for UTC
        stdISO8601SecondsTZ      // "Z070000"
        stdISO8601ShortTZ        // "Z07"
        stdISO8601ColonTZ        // "Z07:00" // prints Z for UTC
        stdISO8601ColonSecondsTZ // "Z07:00:00"
        stdNumTZ                 // "-0700"  // always numeric
        stdNumSecondsTz          // "-070000"
        stdNumShortTZ            // "-07"    // always numeric
        stdNumColonTZ            // "-07:00" // always numeric
        stdNumColonSecondsTZ     // "-07:00:00"
    )


## Comparing Time
Sometime you will need to know, with 2 dates objects, if there are corresponding to the same date, or find which date is after the other.

In **Go**, there is 4 way to compare dates:
- `date1 == date2`, returns `true` when the 2 are the same moment
- `date1 != date2`, returns `true` when the 2 are different moment
- `date1.Before(date2)`, returns `true` when the first is strictly before the second
- `date1.After(date2)`, returns `true` when the first is strictly after the second

> WARNING: When the 2 Time to compare are the same (or correspond to the exact same date), functions `After` and `Before` will return `false`, as a date is neither before nor after itself
> - `date1 == date1`, returns `true`
> - `date1 != date1`, returns `false`
> - `date1.After(date1)`, returns `false`
> - `date1.Before(date1)`, returns `false`

<!-- break -->

> TIPS: If you need to know if a date is before or equal another one, just need to combine the 4 operators
> - `date1 == date2 && date1.After(date2)`, returns `true` when date1 is after or equal date2  
>or using `! (date1.Before(date2))`
> - `date1 == date2 && date1.Before(date2)`, returns `true` when date1 is before or equal date2
>or using `!(date1.After(date2))`

Some examples to see how to use:

    // Init 2 dates for example
    var date1 = time.Date(2009, time.November, 10, 23, 0, 0, 0, time.UTC)
    var date2 = time.Date(2017, time.July, 25, 16, 22, 42, 123, time.UTC)
    var date3 = time.Date(2017, time.July, 25, 16, 22, 42, 123, time.UTC)

    bool1 := date1.Before(date2) // true, because date1 is before date2
    bool2 := date1.After(date2) // false, because date1 is not after date2

    bool3 := date2.Before(date1) // false, because date2 is not before date1
    bool4 := date2.After(date1) // true, because date2 is after date1

    bool5 := date1 == date2 // false, not the same moment
    bool6 := date1 == date3 // true, different objects but representing the exact same time

    bool7 := date1 != date2 // true, different moments
    bool8 := date1 != date3 // false, not different moments

    bool9 := date1.After(date3) // false, because date1 is not after date3 (that are the same)
    bool10:= date1.Before(date3) // false, because date1 is not before date3 (that are the same)

    bool11 := !(date1.Before(date3)) // true, because date1 is not before date3
    bool12 := !(date1.After(date3)) // true, because date1 is not after date3

