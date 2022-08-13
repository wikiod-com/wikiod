---
title: "Append data to existing document"
slug: "append-data-to-existing-document"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

How to append data to an already existing Excel document.

## Appending data
    //the path of the file
    string filePath = "C:\\ExcelDemo.xlsx";
    
    //or if you use asp.net, get the relative path
    filePath = Server.MapPath("ExcelDemo.xlsx");
    
    //create a fileinfo object of an excel file on the disk
    FileInfo file = new FileInfo(filePath);
    
    //create a new Excel package from the file
    using (ExcelPackage excelPackage = new ExcelPackage(file))
    {
        //create an instance of the the first sheet in the loaded file
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets[1];
    
        //add some data
        worksheet.Cells[4, 1].Value = "Added data in Cell A4";
        worksheet.Cells[4, 2].Value = "Added data in Cell B4";
    
        //save the changes
        excelPackage.Save();
    }

