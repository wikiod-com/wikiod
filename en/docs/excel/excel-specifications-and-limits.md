---
title: "Excel specifications and limits"
slug: "excel-specifications-and-limits"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

**Excel specifications and limits** ([Excel **2016-2013**](https://support.office.com/en-us/article/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3#ID0EBABAAA=Excel_2016-2013), [Excel **2010**](https://support.office.com/en-us/article/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3#ID0EBABAAA=Excel_2010), [Excel **2007**](https://support.office.com/en-us/article/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3#ID0EBABAAA=Excel_2007))

---

> **Worksheet and workbook specifications and limits**

|Feature|Maximum limit|
|---|---
|Open workbooks|Limited by available memory and system resources|
|Worksheet size|1,048,576 rows by 16,384 columns|
|Column width|255 characters|
|Row height|409 points|
|Page breaks|1,026 horizontal and vertical|
|Total number of characters that a cell can contain|32,767 characters|
|Characters in a header or footer|255|
|Maximum number of line feeds per cell|253|
|Sheets in a workbook|Limited by available memory (default is 1 sheet)|
|Colors in a workbook|16 million colors (32 bit with full access to 24 bit color spectrum)|
|Named views in a workbook|Limited by available memory|
|Unique cell formats/cell styles|64,000|
|Fill styles|256|
|Line weight and styles|256|
|Unique font types|1,024 global fonts available for use; 512 per workbook|
|Number formats in a workbook|Between 200 and 250, depending on the language version of Excel that you have installed|
|Names in a workbook|Limited by available memory|
|Windows in a workbook|Limited by available memory|
|Hyperlinks in a worksheet|66,530 hyperlinks|
|Panes in a window|4|
|Linked sheets|Limited by available memory|
|Scenarios|Limited by available memory; a summary report shows only the first 251 scenarios|
|Changing cells in a scenario|32|
|Adjustable cells in Solver|200|
|Custom functions|Limited by available memory|
|Zoom range|10 percent to 400 percent|
|Reports|Limited by available memory|
|Sort references|64 in a single sort; unlimited when using sequential sorts|
|Undo levels|100|
|Fields in a data form|32|
|Workbook parameters|255 parameters per workbook|
|Items displayed in filter drop-down lists|10,000|
|Noncontiguous cells that can be selected|2,147,483,648 cells|
|Maximum limits of memory storage and file size for Data Model workbooks|32-bit environment is subject to 2 gigabytes (GB) of virtual address space, shared by Excel, the workbook, and add-ins that run in the same process. A data model’s share of the address space might run up to 500 – 700 megabytes (MB), but could be less if other data models and add-ins are loaded.. 64-bit environment imposes no hard limits on file size. Workbook size is limited only by available memory and system resources.. Excel 2016 offers the Large Address Aware functionality that lets 32-bit Excel 2016 consume twice the memory when users work on a 64-bit Windows operating system. For more information, see Large Address Aware capability change for Excel.. Note: Adding tables to the Data Model increases the file size. If you don’t plan to create complex Data Model relationships using many data sources and data types in your workbook, uncheck the Add this data to the Data Model box when you import or create tables, pivot tables, or data connections.

---

> **Calculation specifications and limits**

|Feature|Maximum limit|
|---|---
|Number precision|15 digits|
|Smallest allowed negative number|-2.23E-308|
|Smallest allowed positive number|2.23E-308|
|Largest allowed positive number|1.00E+308|
|Largest allowed negative number|-1.00E+308|
|Largest allowed positive number via formula|1.7976931348623158e+308|
|Largest allowed negative number via formula|-1.7976931348623158e+308|
|Length of formula contents|8,192 characters|
|Internal length of formula|16,384 bytes|
|Iterations|32,767|
|Worksheet arrays|Limited by available memory|
|Selected ranges|2,048|
|Arguments in a function|255|
|Nested levels of functions|64|
|User defined function categories|255|
|Number of available worksheet functions|341|
|Size of the operand stack|1,024|
|Cross-worksheet dependency|64,000 worksheets that can refer to other sheets|
|Cross-worksheet array formula dependency|Limited by available memory|
|Area dependency|Limited by available memory|
|Area dependency per worksheet|Limited by available memory|
|Dependency on a single cell|4 billion formulas that can depend on a single cell|
|Linked cell content length from closed workbooks|32,767|
|Earliest date allowed for calculation|January 1, 1900 (January 1, 1904, if 1904 date system is used)|
|Latest date allowed for calculation|December 31, 9999|
|Largest amount of time that can be entered|9999:59:59|

---

> **Charting specifications and limits**

|Feature|Maximum limit|
|---|---
|Charts linked to a worksheet|Limited by available memory|
|Worksheets referred to by a chart|255|
|Data series in one chart|255|
|Data points in a data series for 2-D charts|Limited by available memory|
|Data points in a data series for 3-D charts|Limited by available memory|
|Data points for all data series in one chart|Limited by available memory|

---

> **PivotTable and PivotChart report specifications and limits**

|Feature|Maximum limit|
|---|---
|PivotTable reports on a sheet|Limited by available memory|
|Unique items per field|1,048,576|
|Row or column fields in a PivotTable report|Limited by available memory|
|Report filters in a PivotTable report|256 (may be limited by available memory)|
|Value fields in a PivotTable report|256|
|Calculated item formulas in a PivotTable report|Limited by available memory|
|Report filters in a PivotChart report|256 (may be limited by available memory)|
|Value fields in a PivotChart report|256|
|Calculated item formulas in a PivotChart report|Limited by available memory|
|Length of the MDX name for a PivotTable item|32,767|
|Length for a relational PivotTable string|32,767|
|Items displayed in filter drop-down lists|10,000|

Workbooks with the "Allow changes by more than one user..." setting enabled        If the Allow changes by more than one user... setting is on for a workbook, then the following information applies. This setting is accessible by clicking the Review tab > Share Workbook. Note that in newer versions of Excel, the Share Workbook button has been hidden. To unhide it, click File > Options > Quick Access Toolbar. Open the list under Choose commands from and select All Commands. Scroll down that list until you see Share Workbook (Legacy). Select that item and click Add. Click OK. The Share Workbook button is now at the top of the 
Excel window and looks like this: Share Workbook

|Feature|Maximum limit|
|---|---
|Users who can open the file at the same time|256|
|Personal views in the workbook|Limited by available memory|
|Days that change history is maintained|32,767 (default is 30 days)|
|Workbooks that can be merged at one time|Limited by available memory|
|Cells that can be highlighted|32,767|
|Colors used to identify changes made by different users when change highlighting is turned on|32 (each user is identified by a separate color; changes made by the current user are highlighted with navy blue)|
|Excel tables in the workbook|0 (zero). Note: A workbook that contains one or more Excel tables cannot have the Allow changes by more than one user... setting enabled.|

---

**Excel specifications and limits** ([Excel **2016-2013**](https://support.office.com/en-us/article/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3#ID0EBABAAA=Excel_2016-2013), [Excel **2010**](https://support.office.com/en-us/article/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3#ID0EBABAAA=Excel_2010), [Excel **2007**](https://support.office.com/en-us/article/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3#ID0EBABAAA=Excel_2007))


## Excel specifications
Excel specifications and limits ([Excel **2016-2013**](https://support.office.com/en-us/article/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3#ID0EBABAAA=Excel_2016-2013), [Excel **2010**](https://support.office.com/en-us/article/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3#ID0EBABAAA=Excel_2010), [Excel **2007**](https://support.office.com/en-us/article/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3#ID0EBABAAA=Excel_2007))

