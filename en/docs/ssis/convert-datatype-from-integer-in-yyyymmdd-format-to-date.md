---
title: "Convert datatype from Integer in YYYYMMDD format to Date"
slug: "convert-datatype-from-integer-in-yyyymmdd-format-to-date"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Using Built in conversion
Using Derived column we can prepare the input. We will provide yyyy-MM-dd to the final conversion:

 - Year: (DT_STR,4,1252)(DataDate / 10000) 
 - Month: (DT_STR,2,1252)(DataDate / 100 % 100)
 - Day: (DT_STR,2,1252)(DataDate % 100)

All together: (DT_DBDATE)((DT_STR,4,1252)(DataDate / 10000) + "-" + (DT_STR,2,1252)(DataDate / 100 % 100) + "-" + (DT_STR,2,1252)(DataDate % 100))

[![Derived Column Solution][1]][1]


  [1]: http://i.stack.imgur.com/yJ5Hl.jpg

This is a faster solution than a scripting components, but less readable.

## Using Scripting Component
Using c# or vb.net code the conversion is even more simple. An output column is needed because we type can not be changed on the fly, alternative is adding an input column on forehand make it ReadWrite.

[![Adding output column][1]][1]


Next code will fill the new column.

    public override void Input0_ProcessInputRow(Input0Buffer Row)
    {
        if (Row.DataDate_IsNull)
            Row.DataDateAsDate_IsNull = true;
        else 
        {
            DateTime tmp;
            if (DateTime.TryParseExact(Row.DataDate.ToString(), "yyyyMMdd", new DateTimeFormatInfo(), System.Globalization.DateTimeStyles.None, out tmp))
                Row.DataDateAsDate = tmp;
            else
                // throw exception or return null
                Row.DataDateAsDate_IsNull = true;
        }
    }


[![Output scripting component][2]][2]


  [1]: http://i.stack.imgur.com/5DvO7.jpg
  [2]: http://i.stack.imgur.com/9Sr4Q.jpg

