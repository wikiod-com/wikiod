---
title: "Formatting Strings"
slug: "formatting-strings"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Format a string resource
You can add wildcards in string resources and populate them at runtime:

1. Edit strings.xml

       <string name="my_string">This is %1$s</string>

2. Format string as needed

       String fun = "fun";
       context.getString(R.string.my_string, fun);

## Formatting data types to String and vise versa
**Data types to string formatting**

Data types like int, float, double, long, boolean can be formatted to string using String.valueOf().

    String.valueOf(1); //Output -> "1"
    String.valueOf(1.0); //Output -> "1.0"
    String.valueOf(1.2345); //Output -> "1.2345"
    String.valueOf(true); //Output -> "true"

Vise versa of this, formatting string to other data type

    Integer.parseInt("1"); //Output -> 1
    Float.parseFloat("1.2"); //Output -> 1.2
    Boolean.parseBoolean("true"); //Output -> true



## Format a timestamp to string
For full description of patterns, see [SimpleDateFormat reference][1]


    Date now = new Date();
    long timestamp = now.getTime();
    SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy", Locale.US);
    String dateStr = sdf.format(timestamp);


  [1]: https://developer.android.com/reference/java/text/SimpleDateFormat.html

