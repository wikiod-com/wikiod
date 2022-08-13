---
title: "Datepicker"
slug: "datepicker"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Initialization
The **datepicker** is used to show an interactive date selector which is tied to a standard form input field. It makes selecting of date for input tasks very easy and also it is also highly configurable. 

 Any input field can be bound with jquery-ui datepicker by the input field's selector (id,class etc.) using **datepicker()** method like this -

    <input type="text" id="datepicker">
    <script>
        $("#datepicker").datepicker();
    </script>

 Live demo is [here][1].

  [1]: https://jsfiddle.net/ni8mr/tm7bdvas/
  

## Set a custom date format
**Default date format:** "mm/dd/yy"

The following example shows how you can set the date format in initialization with the dateFormat option.

    <input type="text" id="datepicker">
    <script>
        $("#datepicker").datepicker({
            dateFormat: "yy-mm-dd"
        });
    </script>

The following example shows how you can set the date format after initialization with the dateFormat option.


    <input type="text" id="datepicker">
    <script>
        $("#datepicker").datepicker( "option", "dateFormat", "yy-mm-dd" );
    </script>

You can use combinations of the following:

    d - day of month (no leading zero)
    dd - day of month (two digit)
    o - day of the year (no leading zeros)
    oo - day of the year (three digit)
    D - day name short
    DD - day name long
    m - month of year (no leading zero)
    mm - month of year (two digit)
    M - month name short
    MM - month name long
    y - year (two digit)
    yy - year (four digit)
    @ - Unix timestamp (ms since 01/01/1970)
    ! - Windows ticks (100ns since 01/01/0001)
    '...' - literal text
    '' - single quote
    anything else - literal text

Or predefined standard:

    ATOM - 'yy-mm-dd' (Same as RFC 3339/ISO 8601)
    COOKIE - 'D, dd M yy'
    ISO_8601 - 'yy-mm-dd'
    RFC_822 - 'D, d M y' (See RFC 822)
    RFC_850 - 'DD, dd-M-y' (See RFC 850)
    RFC_1036 - 'D, d M y' (See RFC 1036)
    RFC_1123 - 'D, d M yy' (See RFC 1123)
    RFC_2822 - 'D, d M yy' (See RFC 2822)
    RSS - 'D, d M y' (Same as RFC 822)
    TICKS - '!'
    TIMESTAMP - '@'
    W3C - 'yy-mm-dd' (Same as ISO 8601)

A default date format can be applied to all datepickers using the following example:

    <script>
        $.datepicker.setDefaults({
            dateFormat: "yy-mm-dd"
        });
    </script>



## Setting Minimum and Maximum dates for a datepicker
    <script>
    $( ".inclas").datepicker({
      minDate: new Date(2007, 1 - 1, 1)
      maxDate: new Date(2008, 1 - 1, 1)
    });
    </script>
    
    <input type ="text" id="datepick" class="inclas">

## Show week of the year
   The following code will show week of the year number on the left side of the datepicker. By default the week start on Monday, but it can be customized using `firstDay` option. The first week of the year contains the first Thursday of the year, following the ISO 8601 definition.

    <input type="text" id="datepicker">
    <script>
        $("#datepicker").datepicker({
            showWeek: true
        });
    </script>

## Show month and year dropdown
jQuery datepicker has two options to allow displaying dropdowns for month and year selection. These options make navigation through large timeframes easier.

    <input type="text" id="datepicker">
    <script>
        $("#datepicker").datepicker({
            changeMonth: true, // shows months dropdown
            changeYear: true   // shows years dropdown
        });
    </script>

