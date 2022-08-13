---
title: "Getting started with epplus"
slug: "getting-started-with-epplus"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Download the files from [CodePlex][1] and add them to the project.

Or install the files with the Package Manager.

    PM> Install-Package EPPlus 


  [1]: http://epplus.codeplex.com/

## Getting started
<!-- language: lang-c# -->
    //Create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
       //Set some properties of the Excel document
       excelPackage.Workbook.Properties.Author = "VDWWD";
       excelPackage.Workbook.Properties.Title = "Title of Document";
       excelPackage.Workbook.Properties.Subject = "EPPlus demo export data";
       excelPackage.Workbook.Properties.Created = DateTime.Now;

        //Create the WorkSheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //Add some text to cell A1
        worksheet.Cells["A1"].Value = "My first EPPlus spreadsheet!";
        //You could also use [line, column] notation:
        worksheet.Cells[1,2].Value = "This is cell B1!";

        //Save your file
        FileInfo fi = new FileInfo(@"Path\To\Your\File.xlsx");
        excelPackage.SaveAs(fi);
    }

    //Opening an existing Excel file
    FileInfo fi = new FileInfo(@"Path\To\Your\File.xlsx");
    using (ExcelPackage excelPackage = new ExcelPackage(fi))
    {
        //Get a WorkSheet by index. Note that EPPlus indexes are base 1, not base 0!
        ExcelWorksheet firstWorksheet = excelPackage.Workbook.Worksheets[1];
        
        //Get a WorkSheet by name. If the worksheet doesn't exist, throw an exeption
        ExcelWorksheet namedWorksheet = excelPackage.Workbook.Worksheets["SomeWorksheet"];

        //If you don't know if a worksheet exists, you could use LINQ,
        //So it doesn't throw an exception, but return null in case it doesn't find it
        ExcelWorksheet anotherWorksheet = 
            excelPackage.Workbook.Worksheets.FirstOrDefault(x=>x.Name=="SomeWorksheet");
    
        //Get the content from cells A1 and B1 as string, in two different notations
        string valA1 = firstWorksheet.Cells["A1"].Value.ToString();
        string valB1 = firstWorksheet.Cells[1,2].Value.ToString();

        //Save your file
        excelPackage.Save();
    }

