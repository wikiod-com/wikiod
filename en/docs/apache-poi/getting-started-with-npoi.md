---
title: "Getting started with NPOI"
slug: "getting-started-with-npoi"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

It is .NET version of POI Java project. it allows to read/write xls, doc, ppt files without Microsoft Office installed.
Details about documentation is available here:https://github.com/tonyqus/npoi

## Installing NPOI
Best way to include all library related to NPOI is NUGet Package Manager. Search for NPOI on NUGet package manager window.

[![enter image description here][1]][1]


 Once it is successfully installed all needed library will appear in reference section of your current project
[![enter image description here][2]][2]

Then include the NPOI into your file like this 

    using NPOI.SS.UserModel;
    using NPOI.SS.Util;
    using NPOI.XSSF.UserModel




  [1]: https://i.stack.imgur.com/AJi6b.png
  [2]: https://i.stack.imgur.com/H0bM9.png

## Create a Excel file
    MemoryStream excelMS =  GetExcelFile();
     
     //Using Resposne Stream to Make File Available for User to Download;
     Response.Clear();
     Response.ContentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
     Response.AddHeader("Content-Disposition", string.Format("attachment;filename={0}", @"Excel_" + DateTime.Now.ToString("yyyy-dd-M-HH-mm") + ".xlsx"));
    
     Response.BinaryWrite(excelMS.ToArray());
     Response.End();

   

>  be careful while using MIME type. you'll have select different MIME
> types for different file formats. For EX
>     xltm  extension  it will be "application/vnd.ms-excel.template.macroEnabled.12";
>     xlxs  extension  it will be "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
>     Different MIME types supported MS is at this [link][1].

    public MemoryStream GetExcelFile()
    {
        //Excel File Stream 
        MemoryStream ms = null;
        // create workbook
        XSSFWorkbook workbook = new XSSFWorkbook();
        // the table named mySheet
        //Assigning New Sheet Name /
        XSSFSheet sheet = (XSSFSheet)workbook.CreateSheet("WorkSheet");
           
        // Assuming FreezePaneRow  = 10, FreezePaneColumn = 11 in the config file
        int freezeRow = Convert.ToInt32(ConfigurationManager.AppSettings["FreezePaneRow"]);
        int freezeCol = Convert.ToInt32(ConfigurationManager.AppSettings["FreezePaneCol"]);
        try
        {
            // Freeze Created Excel Sheet Row;
                sheet.CreateFreezePane(freezeCol, freezeRow, freezeCol, freezeRow);
            //Freezing only the Header Row;
                sheet.CreateFreezePane(0, 1);
    
             using (ms = new MemoryStream())
                {
                    logger.Info("Using Memory Stream to Create New WorkBook");
                    workbook.Write(ms); // Write to memory stream for download through browser     
                }
        }
        catch (Exception Ex)
        { ... }
        return ms;
    }

 


  [1]: https://blogs.msdn.microsoft.com/vsofficedeveloper/2008/05/08/office-2007-file-format-mime-types-for-http-content-streaming-2/



## Reading a Excel File
There are various approach to read a excel file. I'll provide a example with file path parameter. You can get various way to read a file at [this][1] post.

    public void ReadExcel(string path)
            {
                // Write data in workbook from xls document.
                XSSFWorkbook workbook = new XSSFWorkbook(path);
                // Read the current table data
                XSSFSheet sheet = (XSSFSheet)workbook.GetSheetAt(0);
                // Read the current row data
                XSSFRow headerRow = (XSSFRow)sheet.GetRow(0);
                // LastCellNum is the number of cells of current rows
                int cellCount = headerRow.LastCellNum;
                DataTable dt = new DataTable();
                bool isBlanKRow = false;
    
                try
                {
                    if (dt.Rows.Count == 0)
                    {
                        //Reading First Row as Header for Excel Sheet;
                        try
                        {
                            for (int j = headerRow.FirstCellNum; j < cellCount; j++)
                            {
                                // get data as the column header of DataTable
                                DataColumn column = new DataColumn(headerRow.GetCell(j).StringCellValue);
                                dt.Columns.Add(column);
                            }
                        }
                        catch (Exception Ex)
                        { }
                    }
    
                    for (int sheetindex = 0; sheetindex < workbook.NumberOfSheets; sheetindex++)
                    {
                        sheet = (XSSFSheet)workbook.GetSheetAt(sheetindex);
                        if (null != sheet)
                        {
                            // LastRowNum is the number of rows of current table
                            int rowCount = sheet.LastRowNum + 1;
                            //Reading Rows and Copying it to Data Table;
                            try
                            {
                                for (int i = (sheet.FirstRowNum + 1); i < rowCount; i++)
                                {
                                    XSSFRow row = (XSSFRow)sheet.GetRow(i);
                                    DataRow dataRow = dt.NewRow();
                                    isBlanKRow = true;
                                    try
                                    {
                                        for (int j = row.FirstCellNum; j < cellCount; j++)
                                        {
                                            if (null != row.GetCell(j) && !string.IsNullOrEmpty(row.GetCell(j).ToString()) && !string.IsNullOrWhiteSpace(row.GetCell(j).ToString()))
                                            {
                                                dataRow[j] = row.GetCell(j).ToString();
                                                isBlanKRow = false;
                                            }
    
                                        }
                                    }
                                    catch (Exception Ex)
                                    { }
                                    if (!isBlanKRow)
                                    {
                                        dt.Rows.Add(dataRow);
                                    }
                                }
                            }
                            catch (Exception Ex)
                            { }
                        }
                    }
                }
                catch (Exception Ex)
                { }
                finally
                {
                    workbook.UnlockStructure();
                    workbook.UnlockRevision();
                    workbook.UnlockWindows();
                    workbook = null;
                    sheet = null;
                }
            }

  [1]: https://stackoverflow.com/questions/5855813/npoi-how-to-read-file-using-npoi


