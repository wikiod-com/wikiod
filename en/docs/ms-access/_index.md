---
title : ms-access Tutorial
slug : ms-access-tutorial
weight : 9974
draft : false
images : []
type : docs
---

*Apologies for the wall of text. Future edits will likely add screenshots and other visual elements.*

Queries in Microsoft Access can be created by any one of four methods
1) Using a step-by-step Query Wizard builder through the GUI which will ask you a series of questions about what data you wish to display and how various bits of data are related to each other.
2) By using the GUI in Design View, in which you select tables and specific fields with your mouse. The visual ordering of the various fields can be specified by dragging the relevant columns in the bottom panel and additional properties for each field can be specified in the Properties panel (right).
3) By switching from Design View to SQL View and specifying the raw SQL query code. Under most circumstances you can freely toggle between Design View and SQL View - Access will incorporate changes that you made in one view to the other. The syntax of Access SQL is similar to, but not identical to, that used in MySQL, PostgreSQL, Oracle, or MS SQL Server (tSQL).
4) Using Visual Basic for Applications programming which can be accessed via the Macro group of the Database Tools ribbon (Access 2007+). Database manipulation occurs via the ADO or DAO libraries and uses the same syntax as SQL View in the main application, with the exception that various special characters have to be "escaped." Queries created via this method are not accessible directly from the Navigation Pane but must be placed in a function or subprocedure and either triggered by other elements (e.g. by a button in a Form) via Macros or directly executed in the VBA GUI interface.

Editing a record value from a query in datasheet view will result in a change in the underlying record value, assuming the query field is not an aggregation or concatenation of multiple sources of information.

Forms and Reports can be used to display information from queries in a form alternative to a simple "Datasheet" view which appears similar to an Excel-style spreadsheet. Forms are targeted to on-screen display, whereas Reports are targeted to those printed on paper.

