---
title: "Time"
slug: "time"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
- now()
- Dates.today()
- Dates.year(t)
- Dates.month(t)
- Dates.day(t)
- Dates.hour(t)
- Dates.minute(t)
- Dates.second(t)
- Dates.millisecond(t)
- Dates.format(t, s)

## Current Time
To get the current date and time, use the `now` function:

    julia> now()
    2016-09-04T00:16:58.122

This is the local time, which includes the machine's configured time zone. To get the time in the [Coordinated Universal Time (UTC)](https://en.wikipedia.org/wiki/Coordinated_Universal_Time) time zone, use `now(Dates.UTC)`:

    julia> now(Dates.UTC)
    2016-09-04T04:16:58.122

To get the current date, without the time, use `today()`:

    julia> Dates.today()
    2016-10-30

The return value of `now` is a `DateTime` object. There are functions to get the individual components of a `DateTime`:

    julia> t = now()
    2016-09-04T00:16:58.122
    
    julia> Dates.year(t)
    2016
    
    julia> Dates.month(t)
    9
    
    julia> Dates.day(t)
    4
    
    julia> Dates.hour(t)
    0
    
    julia> Dates.minute(t)
    16
    
    julia> Dates.second(t)
    58
    
    julia> Dates.millisecond(t)
    122

It is possible to format a `DateTime` using a specially-formatted format string:

    julia> Dates.format(t, "yyyy-mm-dd at HH:MM:SS")
    "2016-09-04 at 00:16:58"

Since many of the `Dates` functions are exported from the `Base.Dates` [module][1], it can save some typing to write

    using Base.Dates

which then enables accessing the qualified functions above without the `Dates.` qualification.

  [1]: https://www.wikiod.com/julia-lang/modules

