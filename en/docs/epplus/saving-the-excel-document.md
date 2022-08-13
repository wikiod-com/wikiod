---
title: "Saving the Excel document"
slug: "saving-the-excel-document"
draft: false
images: []
weight: 9899
type: docs
toc: true
---

Examples on how to save the created Excel sheet to the Disk or send it to the Browser.

## Save to disk
<!-- language: lang-c# -->

    //Using File.WriteAllBytes
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create a new Worksheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //add some text to cell A1
        worksheet.Cells["A1"].Value = "My second EPPlus spreadsheet!";
    
        //convert the excel package to a byte array
        byte[] bin = excelPackage.GetAsByteArray();
    
        //the path of the file
        string filePath = "C:\\ExcelDemo.xlsx";
    
        //or if you use asp.net, get the relative path
        filePath = Server.MapPath("ExcelDemo.xlsx");
    
        //write the file to the disk
        File.WriteAllBytes(filePath, bin);

        //Instead of converting to bytes, you could also use FileInfo
        FileInfo fi = new FileInfo(filePath);
        excelPackage.SaveAs(fi);
    }

    //Using SaveAs
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create a new Worksheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //add some text to cell A1
        worksheet.Cells["A1"].Value = "My second EPPlus spreadsheet!";
        //the path of the file
        string filePath = "C:\\ExcelDemo.xlsx";
    
        //or if you use asp.net, get the relative path
        filePath = Server.MapPath("ExcelDemo.xlsx");
    
        //Write the file to the disk
        FileInfo fi = new FileInfo(filePath);
        excelPackage.SaveAs(fi);
    }

## Send to the Browser
<!-- language: lang-c# -->
    //create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create the WorkSheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //add some text to cell A1
        worksheet.Cells["A1"].Value = "My second EPPlus spreadsheet!";
    
        //convert the excel package to a byte array
        byte[] bin = excelPackage.GetAsByteArray();
    
        //clear the buffer stream
        Response.ClearHeaders();
        Response.Clear();
        Response.Buffer = true;
    
        //set the correct contenttype
        Response.ContentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
    
        //set the correct length of the data being send
        Response.AddHeader("content-length", bin.Length.ToString());
    
        //set the filename for the excel package
        Response.AddHeader("content-disposition", "attachment; filename=\"ExcelDemo.xlsx\"");
    
        //send the byte array to the browser
        Response.OutputStream.Write(bin, 0, bin.Length);
    
        //cleanup
        Response.Flush();
        HttpContext.Current.ApplicationInstance.CompleteRequest();
    }

## Save to disk with SaveFileDialog
<!-- language: lang-c# -->
    //Using File.WriteAllBytes
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create a new Worksheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //add some text to cell A1
        worksheet.Cells["A1"].Value = "My fourth EPPlus spreadsheet!";
    
        //convert the excel package to a byte array
        byte[] bin = excelPackage.GetAsByteArray();
    
        //create a SaveFileDialog instance with some properties
        SaveFileDialog saveFileDialog1 = new SaveFileDialog();
        saveFileDialog1.Title = "Save Excel sheet";
        saveFileDialog1.Filter = "Excel files|*.xlsx|All files|*.*";
        saveFileDialog1.FileName = "ExcelSheet_" + DateTime.Now.ToString("dd-MM-yyyy") + ".xlsx";
    
        //check if user clicked the save button
        if (saveFileDialog1.ShowDialog() == DialogResult.OK)
        {
            //write the file to the disk
            File.WriteAllBytes(saveFileDialog1.FileName, bin);
        }
    }

    //Using SaveAs
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create a new Worksheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //add some text to cell A1
        worksheet.Cells["A1"].Value = "My fourth EPPlus spreadsheet!";
    
        //create a SaveFileDialog instance with some properties
        SaveFileDialog saveFileDialog1 = new SaveFileDialog();
        saveFileDialog1.Title = "Save Excel sheet";
        saveFileDialog1.Filter = "Excel files|*.xlsx|All files|*.*";
        saveFileDialog1.FileName = "ExcelSheet_" + DateTime.Now.ToString("dd-MM-yyyy") + ".xlsx";
    
        //check if user clicked the save button
        if (saveFileDialog1.ShowDialog() == DialogResult.OK)
        {
            //Get the FileInfo
            FileInfo fi = new FileInfo(saveFileDialog1.FileName);
            //write the file to the disk
            excelPackage.SaveAs(fi);
        }
    }

