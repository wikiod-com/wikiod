---
title: "LocalDate (dealing with dates)"
slug: "localdate-dealing-with-dates"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Introduction and use case
A [`LocalDate`][1] is a date without a timezone. Consider using them when you are only concerned with the year, month, and day of month and are not interested in an exact time. For example, when we write our date of birth (such as 1960-01-01) we don't care about the timezone in which it happened. 

[1]:http://www.joda.org/joda-time/apidocs/org/joda/time/LocalDate.html

## Basic LocalDates

A given year-month-day:

    LocalDate oneJanuaryNineteenSixty = new LocalDate(1960,1,1);

Today's date:

    LocalDate today = LocalDate.now()

Tomorrow:

    LocalDate tomorrow = LocalDate.now().plusDays(1);

Yesterday:

    LocalDate yesterday = LocalDate.now().minusDays(1);

Two weeks ago:

    LocalDate twoWeeksAgo = LocalDate.now().minusWeeks(2);

Nine months before 1960-01-01:

    LocalDate conception = new LocalDate(1960-01-01).minusMonths(9);

The Saturday of this week:

    LocalDate saturday = LocalDate.now().withDayOfWeek(DateTimeConstants.SATURDAY);




## Converting other types into LocalDates
Converting a `java.util.Calendar` object:

    Calendar rightNow = Calendar.getInstance();
    LocalDate today = LocalDate.fromCalendarFields(rightNow);

Converting a `java.util.Date` object:

    Date rightNow = new Date();
    LocalDate today = LocalDate.fromDateFields(rightNow);

Converting a string:

    String dateString = "1960-01-01"
    LocalDate birthDate = LocalDate.parse(dateString);



