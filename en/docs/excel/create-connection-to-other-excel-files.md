---
title: "Create Connection to other Excel Files"
slug: "create-connection-to-other-excel-files"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Creating an Connection to other Excel Files to query
There are many fine ways to get this done, which others have already suggested. Following along the "get Excel data via SQL track", here are some pointers.

Excel has the "Data Connection Wizard" which allows you to import or link from another data source or even within the very same Excel file.
As part of Microsoft Office (and OS's) are two providers of interest: the old "Microsoft.Jet.OLEDB", and the latest "Microsoft.ACE.OLEDB". Look for them when setting up a connection (such as with the Data Connection Wizard).
Once connected to an Excel workbook, a worksheet or range is the equivalent of a table or view. The table name of a worksheet is the name of the worksheet with a dollar sign ("$") appended to it, and surrounded with square brackets ("[" and "]"); of a range, it is simply the name of the range. To specify an unnamed range of cells as your recordsource, append standard Excel row/column notation to the end of the sheet name in the square brackets.

Naming the range of the data you wish to query using the name manager is very helpfull, as naming it "Database" can treat the data in a worksheet like a database table, allowiny you to run SQL statements on your data for easy access.
It is also worth noting that the named range should not have blank or missing data, as it will cause the SQL to break.

The native SQL will (more or less be) the SQL of Microsoft Access. (In the past, it was called JET SQL; however Access SQL has evolved, and I believe JET is deprecated old tech.)
Example, reading a worksheet: SELECT * FROM [Sheet1$]
Example, reading a range: SELECT * FROM MyRange
Example, reading an unnamed range of cells: SELECT * FROM [Sheet1$A1:B10]
There are many many many books and web sites available to help you work through the particulars.

