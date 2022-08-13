---
title: "Pivot Table"
slug: "pivot-table"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Pivot table is one kind of interactive table, which can be used to calculate data, such as get sum or count data. Also, users can change pivot table layout for analyzing data with different ways or reassign row/column label. Every time users change layout, data will be recalculated in pivot table.

## Creating a Pivot Table
    //create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create 2 WorkSheets. One for the source data and one for the Pivot table
        ExcelWorksheet worksheetPivot = excelPackage.Workbook.Worksheets.Add("Pivot");
        ExcelWorksheet worksheetData = excelPackage.Workbook.Worksheets.Add("Data");
    
        //add some source data
        worksheetData.Cells["A1"].Value = "Column A";
        worksheetData.Cells["A2"].Value = "Group A";
        worksheetData.Cells["A3"].Value = "Group B";
        worksheetData.Cells["A4"].Value = "Group C";
        worksheetData.Cells["A5"].Value = "Group A";
        worksheetData.Cells["A6"].Value = "Group B";
        worksheetData.Cells["A7"].Value = "Group C";
        worksheetData.Cells["A8"].Value = "Group A";
        worksheetData.Cells["A9"].Value = "Group B";
        worksheetData.Cells["A10"].Value = "Group C";
        worksheetData.Cells["A11"].Value = "Group D";
    
        worksheetData.Cells["B1"].Value = "Column B";
        worksheetData.Cells["B2"].Value = "emc";
        worksheetData.Cells["B3"].Value = "fma";
        worksheetData.Cells["B4"].Value = "h2o";
        worksheetData.Cells["B5"].Value = "emc";
        worksheetData.Cells["B6"].Value = "fma";
        worksheetData.Cells["B7"].Value = "h2o";
        worksheetData.Cells["B8"].Value = "emc";
        worksheetData.Cells["B9"].Value = "fma";
        worksheetData.Cells["B10"].Value = "h2o";
        worksheetData.Cells["B11"].Value = "emc";
    
        worksheetData.Cells["C1"].Value = "Column C";
        worksheetData.Cells["C2"].Value = 299;
        worksheetData.Cells["C3"].Value = 792;
        worksheetData.Cells["C4"].Value = 458;
        worksheetData.Cells["C5"].Value = 299;
        worksheetData.Cells["C6"].Value = 792;
        worksheetData.Cells["C7"].Value = 458;
        worksheetData.Cells["C8"].Value = 299;
        worksheetData.Cells["C9"].Value = 792;
        worksheetData.Cells["C10"].Value = 458;
        worksheetData.Cells["C11"].Value = 299;
    
        worksheetData.Cells["D1"].Value = "Column D";
        worksheetData.Cells["D2"].Value = 40075;
        worksheetData.Cells["D3"].Value = 31415;
        worksheetData.Cells["D4"].Value = 384400;
        worksheetData.Cells["D5"].Value = 40075;
        worksheetData.Cells["D6"].Value = 31415;
        worksheetData.Cells["D7"].Value = 384400;
        worksheetData.Cells["D8"].Value = 40075;
        worksheetData.Cells["D9"].Value = 31415;
        worksheetData.Cells["D10"].Value = 384400;
        worksheetData.Cells["D11"].Value = 40075;
    
        //define the data range on the source sheet
        var dataRange = worksheetData.Cells[worksheetData.Dimension.Address];
    
        //create the pivot table
        var pivotTable = worksheetPivot.PivotTables.Add(worksheetPivot.Cells["B2"], dataRange, "PivotTable");
    
        //label field
        pivotTable.RowFields.Add(pivotTable.Fields["Column A"]);
        pivotTable.DataOnRows = false;
    
        //data fields
        var field = pivotTable.DataFields.Add(pivotTable.Fields["Column B"]);
        field.Name = "Count of Column B";
        field.Function = DataFieldFunctions.Count;
    
        field = pivotTable.DataFields.Add(pivotTable.Fields["Column C"]);
        field.Name = "Sum of Column C";
        field.Function = DataFieldFunctions.Sum;
        field.Format = "0.00";
    
        field = pivotTable.DataFields.Add(pivotTable.Fields["Column D"]);
        field.Name = "Sum of Column D";
        field.Function = DataFieldFunctions.Sum;
        field.Format = "€#,##0.00";
    }

