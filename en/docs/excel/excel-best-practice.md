---
title: "Excel best practice"
slug: "excel-best-practice"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Use Excel Tables
By selecting a matrix and choosing "Insert Table" from the menu, you create a table which allows you to pull and insert data in a structured way. Let's say you have named the table "SalesEvents" and given that the first (header) row reads "Salesperson" "Date" "Sales Amount", you can calculate sales amount like this: `=SUM(SalesEvents[Sales Amount])`. Entering data at the bottom of the table automatically adds new rows. This is a very good way of enhancing structure by encapsulating deltails inside your tables, leaving outside cells good posibilities to extract the high-level properties like column sums. 

## Pull data from PivotTables with GetPivotData
Sometimes it is hard to make all the data of your Pivot Table confirm to the reporting format you have to present your crunched data into. Then use GetPivotData! It has an automatic fill in of arguments that you easy can learn from and it lets you through its parameters flexibly choose and pick from all the visible fields in your Pivot Tables.


## Use Pivot Tables
When you have an Excel Table, and not only then, it is easy to use data as input in a PivotTable which will provide most of the analysis you would need on it. Learn to use it you won't regret it! It could replaces tons of user designed cell Formulas and it is fast and much easier to document.


## Best Practices
Here are some basic best practices for Excel:

- Flat File Database - Excel **IS** a Flat File application and should be treated as such

- Less Worksheet/Workbooks is more. Analyzing will be much faster with fewer worksheets/workbooks to go through. Try to ensure that all raw data is on one worksheet in one workbook
- The layout should consist of 1 sheet of raw data, your final data will come from this.
- Make headers **BOLD** this helps Excel recognize them as headers for things like sort
- When putting data into the data area of your spreadsheet try to avoid blank rows and columns. Excel might consider a blank row or column to be the end of your data. *It would be a good idea to leave the top 4 rows blank above your headers to use for totals instead of having them at the bottom*
- Sort your data whenever possible. This will  help in speeding up some of the formulas and calculations that you have in the spreadsheet
- Use real dates for headings and then format them appropriately. By this I mean if you need month names for your headers use 1/1/2017, 1/2/2017, 1/3/2017 and then format them as "Mmmm". This is very simple nad will make life easier down the road when used in formulas.
- Don't put into 1 cell what can go into more than one. Meaning if you are making a list of users full names you should have the first name in one column and the last name in the next column (and possibly more columns for middle names and suffixes). It is easier to do this from the beginning than to try and do this with formulas later.
- Place your headings across the columns and your data in rows directly beneath. Excel has far more rows than it has columns. To make you spreadsheet future proof your raw data should have the data going down the rows so that it can continue well past the number of available columns.

