---
title: "Formatting values"
slug: "formatting-values"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

How to get the desired formatting of DateTime and Numeric values.

## Number formatting
<!-- language: lang-c# -->
    //integer (not really needed unless you need to round numbers, Excel with use default cell properties)
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = "0";
    
    //integer without displaying the number 0 in the cell
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = "#";
    
    //number with 1 decimal place
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = "0.0";
    
    //number with 2 decimal places
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = "0.00";
    
    //number with 2 decimal places and thousand separator
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = "#,##0.00";
    
    //number with 2 decimal places and thousand separator and money symbol
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = "€#,##0.00";
    
    //percentage (1 = 100%, 0.01 = 1%)
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = "0%";

## Date formatting
<!-- language: lang-c# -->
    //default DateTime patterns
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = DateTimeFormatInfo.CurrentInfo.ShortDatePattern;
    
    //custom DateTime patters
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = "dd-MM-yyyy HH:mm";
    
    //or overwrite the patterns in the CurrentThread with your own
    Thread.CurrentThread.CurrentCulture = new CultureInfo("nl-NL")
    {
        DateTimeFormat = { YearMonthPattern = "MMM yy" }
    };
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = DateTimeFormatInfo.CurrentInfo.YearMonthPattern;

## Text Format
<!-- language: lang-c# -->
    worksheet.Cells["A1:A25"].Style.Numberformat.Format = "@";

