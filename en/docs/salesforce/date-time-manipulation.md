---
title: "Date Time Manipulation"
slug: "date-time-manipulation"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Easily Find Last Day of a Month
If you need to find the last day of the month, you can do complicated DateTime gymnastics or you can use the following method.

Say you want to find the last day of February 2021. Do the following:

    Integer month = 2;
    Integer day = null;
    Integer year = 2021;

    // Create a new DateTime object for the first day of the month following
    // the date you're looking for.
    DateTime dtTarget = DateTime.newInstance(year, month, 1);
    //In this case we would be sure that month would not be out of bound
    dtTarget = dtTarget.addMonths(1);
    // Then use the .addDays() method to add negative 1 days. This gives you
    // the last day of the target month.
    DateTime lastDayOfMonth = dtTarget.addDays(-1);
    day = lastDayOfMonth.day();

    System.debug(lastDayOfMonth);
    System.debug(day);

This produces the following output in the Logs:

    18:19:57:005 USER_DEBUG [15]|DEBUG|2021-02-28 08:00:00
    18:21:10:003 USER_DEBUG [16]|DEBUG|28

This works for all the add methods, allowing you easily and quickly find DateTimes in the past.
    

