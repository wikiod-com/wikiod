---
title: "Tables"
slug: "tables"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

This topic describe how to add and style tables

## Adding and formating a table
    //Using statement for ExcelTable and TableStyles
    using OfficeOpenXml.Table;


    //Defining the tables parameters
    int firstRow =1;
    int lastRow = worksheet.Dimension.End.Row;
    int firstColumn = 1;
    int lastColumn = worksheet.Dimension.End.Column;
    ExcelRange rg = worksheet.Cells[firstRow, firstColumn, lastRow, LastColumn];
    string tableName = "Table1";

    //Ading a table to a Range
    ExcelTable tab = worksheet.Tables.Add(rg, tableName);
    
    //Formating the table style
    tab.TableStyle = TableStyles.Light8;

