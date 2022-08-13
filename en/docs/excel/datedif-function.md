---
title: "DATEDIF function"
slug: "datedif-function"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Syntax
- =DATEDIF(start_date,end_date,unit) 

## Parameters
| Unit   | Returns |
| ------ | ------ |
| "Y"    | The number of complete years in the period   | 
| "M"    | The number of complete months in the period   |
| "D"    | The number of days in the period   |
| "MD"   | The difference between the days in start_date and end_date. The months and       years of the dates are ignored   |
| "YM"    | The difference between the months in start_date and end_date. The days and years of the dates are ignored   |
| "YD"     | The difference between the days of start_date and end_date. The years of the dates are ignored   |

Be careful of Leap Year calculations when the units ignore years.  For example:

    =datedif("2010-01-01","2010-07-21","YD")

returns 201 days

    =datedif("2016-01-01","2016-07-21","YD")

returns 202 days

## Period count between dates
The `DATEDIF` function returns the difference between two date values, based on the interval specified. It is provided for compatibility with Lotus 1-2-3. The `DATEDIF` function cannot be found on the function list and autocomplete and screen tips are unavailable. *Note: It is pronounced "date diff" rather than "dated if"*.   

<hr/>

    =datedif("2010-01-01","2016-07-21","D")

returns the number of days (**2393**) between the two dates

    =datedif("2010-01-01","2016-07-21","M")

returns the number of months (**78**) between the two dates

    =datedif("2010-01-01","2016-07-21","Y")
returns the number of years (**6**) between the two dates

    =datedif("2010-01-01","2016-07-21","MD")
returns the number of days (**20**) between the two dates-ignoring the months and years

    =datedif("2010-01-01","2016-07-21","YM")
returns the number of months (**6**) between the two dates-ignoring the years

    =datedif("2010-01-01","2016-07-21","YD")
returns the number of days (**201**) between the two dates-ignoring the years 

