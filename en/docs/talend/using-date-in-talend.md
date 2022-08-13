---
title: "Using Date in Talend"
slug: "using-date-in-talend"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Parsing a date
Parsing date is used when having an input typed as `String` and when it is needed to get it as a `Date`.
The class `TalendDate` contains method `TalendDate.parseDate("pattern","stringDate")`.

Pattern here is the **input** pattern, and not the expected output pattern.

Usage : 
For an input string like `"2017-05-03 17:09:00"` , the call will be :

    TalendDate.parseDate("yyyy-MM-dd HH:mm:ss","2017-05-03 17:09:00")

The result could be a date like :

    2017-05-03 17:09:00 
or

    03/05/2017
Depending on the **output pattern** which is defined outside the parseDate method.

[![Define date pattern in the output settings of the schema][1]][1]


  [1]: https://i.stack.imgur.com/CBgzQ.png

## Automatic date parsing
Since Talend 6.3 , an option in tMap allows to automatically convert types. 
When activated, output pattern is used as the expected input pattern to automatically convert data.

First, activate the option : 

[![Modify default settings][1]][1]

[![Enable auto-conversion][2]][2]

Then modify output pattern, used as the input pattern :

[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/WTsFz.png
  [2]: https://i.stack.imgur.com/lenUV.png
  [3]: https://i.stack.imgur.com/TyB5i.png

