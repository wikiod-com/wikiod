---
title: "Creating formulas and calculate ranges"
slug: "creating-formulas-and-calculate-ranges"
draft: false
images: []
weight: 9712
type: docs
toc: true
---

Basic examples of how to create cells with a formula for calculations within the Excel sheet.

## Add formulas to a cell
<!-- language-all: lang-c# -->
    //set the total value of cells in range A1 - A25 into A27
    worksheet.Cells["A27"].Formula = "=SUM(A1:A25)";
    
    //set the number of cells with content in range C1 - C25 into C27
    worksheet.Cells["C27"].Formula = "=COUNT(C1:C25)";
    
    //fill column K with the sum of each row, range A - J
    for (int i = 1; i <= 25; i++)
    {
        var cell = worksheet.Cells[i, 12];
        cell.Formula = "=SUM(" + worksheet.Cells[i, 1].Address + ":" + worksheet.Cells[i, 10].Address + ")";
    }
    
    //calculate the quartile of range E1 - E25 into E27
    worksheet.Cells[27, 5].Formula = "=QUARTILE(E1:E25,1)";

## Complete example with formulas
<!-- language-all: lang-c# -->
    //create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create 2 WorkSheets
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
        ExcelWorksheet worksheet2 = excelPackage.Workbook.Worksheets.Add("Sheet 2");
    
        //set the calculation mode to manual
        excelPackage.Workbook.CalcMode = ExcelCalcMode.Manual;
    
        //fill cell data with a loop, note that row and column indexes start at 1
        for (int i = 1; i <= 25; i++)
        {
            for (int j = 1; j <= 10; j++)
            {
                worksheet.Cells[i, j].Value = (i + j) - 1;
                worksheet2.Cells[i, j].Value = (i + j) - 1;
            }
        }
    
        //set the total value of cells in range A1 - A25 into A27
        worksheet.Cells["A27"].Formula = "=SUM(A1:A25)";
    
        //set the number of cells with content in range C1 - C25 into C27
        worksheet.Cells["C27"].Formula = "=COUNT(C1:C25)";
    
        //fill column K with the sum of each row, range A - J
        for (int i = 1; i <= 25; i++)
        {
            var cell = worksheet.Cells[i, 12];
            cell.Formula = "=SUM(" + worksheet.Cells[i, 1].Address + ":" + worksheet.Cells[i, 10].Address + ")";
        }
    
        //calculate the quartile of range E1 - E25 into E27
        worksheet.Cells[27, 5].Formula = "=QUARTILE(E1:E25,1)";
    
        //set the total value of all cells in Sheet 2 into G27
        worksheet.Cells["G27"].Formula = "=SUM('" + worksheet2.Name + "'!" + worksheet2.Dimension.Start.Address + ":" + worksheet2.Dimension.End.Address + ")";
    
        //set the number of cells with content in Sheet 2, range C1 - C25 into I27
        worksheet.Cells["I27"].Formula = "=COUNT('" + excelPackage.Workbook.Worksheets[2].Name + "'!" + excelPackage.Workbook.Worksheets[2].Cells["A1:B25"] + ")";
    
        //calculate all the values of the formulas in the Excel file
        excelPackage.Workbook.Calculate();

        //Save the file
        FileInfo fi = new FileInfo("FormulaExample.xlsx");
        excelPackage.SaveAs(fi);
    }

## Formula with multiple sheets
<!-- language-all: lang-c# -->
    //set the total value of all cells in Sheet 2 into G27
    worksheet.Cells["G27"].Formula = "=SUM('" + worksheet2.Name + "'!" + worksheet2.Dimension.Start.Address + ":" + worksheet2.Dimension.End.Address + ")";
    
    //set the number of cells with content in Sheet 2, range C1 - C25 into I27
    worksheet.Cells["I27"].Formula = "=COUNT('" + excelPackage.Workbook.Worksheets[2].Name + "'!" + excelPackage.Workbook.Worksheets[2].Cells["A1:B25"] + ")";

## Manual calculation
<!-- language-all: lang-c# -->
If you use formulas, Excel will ask you to save the file every time, even if there were no changes made. To prevent this behaviour you can set the calculation mode to manual.

    excelPackage.Workbook.CalcMode = ExcelCalcMode.Manual;
    
    //fill the sheet with data and set the formulas
    
    excelPackage.Workbook.Calculate();

