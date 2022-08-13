---
title: "Columns and Rows"
slug: "columns-and-rows"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

This topic contains information about working with columns and rows, like resizing, hiding, autofit

## Autofit columns
<!-- language-all: lang-c# -->
    //Make all text fit the cells
    worksheet.Cells[worksheet.Dimension.Address].AutoFitColumns();

    //Autofit with minimum size for the column.
    double minimumSize = 10;
    worksheet.Cells[worksheet.Dimension.Address].AutoFitColumns(minimumSize);    

    //Autofit with minimum and maximum size for the column.
    double maximumSize = 50;
    worksheet.Cells[worksheet.Dimension.Address].AutoFitColumns(minimumSize, maximumSize);

    //optional use this to make all columms just a bit wider, text would sometimes still overflow after AutoFitColumns().
    for (int col = 1; col <= worksheet.Dimension.End.Column; col++)
    {
        worksheet.Column(col).Width = worksheet.Column(col).Width + 1;
    }

## Hide columns and rows
<!-- language-all: lang-c# -->
    //Hide column "A"
    worksheet.Column(1).Hidden = true;

    //Hide row 1
    worksheet.Row(1).Hidden = true;

## Resizing rows and columns
<!-- language-all: lang-c# -->
    //Set the row "A" height to 15
    double rowHeight = 15;
    worksheet.Row(1).Height = rowHeight;
    
    //Set the column 1 width to 50
    double columnWidth = 50;
    worksheet.Column(1).Width = columnWidth;

When Bestfit is set to true, the column will grow wider when a user inputs numbers in a cell

    worksheet.Column(1).BestFit = true;

## Copy columns or rows
    workSheet.Cells[1,5,100,5].Copy(workSheet.Cells[1,2,100,2]);

Copies column 5 into column 2 
Basically Source.Copy(Destination) 

This would only copy the first 100 rows. 

    Cells[RowStart, ColumnStart, RowEnd, ColumnEnd ]
    is the format so to copy a row into another row you would just switch the indexes accordingly

