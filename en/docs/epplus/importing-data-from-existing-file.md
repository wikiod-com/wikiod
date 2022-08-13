---
title: "Importing data from existing file"
slug: "importing-data-from-existing-file"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

How to import data from an existing Excel or CSV file.

## Import data from Excel file
    //create a list to hold all the values
    List<string> excelData = new List<string>();
    
    //read the Excel file as byte array
    byte[] bin = File.ReadAllBytes("C:\\ExcelDemo.xlsx");
    
    //or if you use asp.net, get the relative path
    byte[] bin = File.ReadAllBytes(Server.MapPath("ExcelDemo.xlsx"));
    
    //create a new Excel package in a memorystream
    using (MemoryStream stream = new MemoryStream(bin))
    using (ExcelPackage excelPackage = new ExcelPackage(stream))
    {
        //loop all worksheets
        foreach (ExcelWorksheet worksheet in excelPackage.Workbook.Worksheets)
        {
            //loop all rows
            for (int i = worksheet.Dimension.Start.Row; i <= worksheet.Dimension.End.Row; i++)
            {
                //loop all columns in a row
                for (int j = worksheet.Dimension.Start.Column; j <= worksheet.Dimension.End.Column; j++)
                {
                    //add the cell data to the List
                    if (worksheet.Cells[i, j].Value != null)
                    {
                        excelData.Add(worksheet.Cells[i, j].Value.ToString());
                    }
                }
            }
        }
    }

## Import data from CSV file
    //set the formatting options
    ExcelTextFormat format = new ExcelTextFormat();
    format.Delimiter = ';';
    format.Culture = new CultureInfo(Thread.CurrentThread.CurrentCulture.ToString());
    format.Culture.DateTimeFormat.ShortDatePattern = "dd-mm-yyyy";
    format.Encoding = new UTF8Encoding();
    
    //read the CSV file from disk
    FileInfo file = new FileInfo("C:\\CSVDemo.csv");
    
    //or if you use asp.net, get the relative path
    FileInfo file = new FileInfo(Server.MapPath("CSVDemo.csv"));
    
    //create a new Excel package
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create a WorkSheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //load the CSV data into cell A1
        worksheet.Cells["A1"].LoadFromText(file, format);
    }

## Import data from Excel file with FileUpload Control
    //check if there is actually a file being uploaded
    if (FileUpload1.HasFile)
    {
        //load the uploaded file into the memorystream
        using (MemoryStream stream = new MemoryStream(FileUpload1.FileBytes))
        using (ExcelPackage excelPackage = new ExcelPackage(stream))
        {
            //loop all worksheets
            foreach (ExcelWorksheet worksheet in excelPackage.Workbook.Worksheets)
            {
                //loop all rows
                for (int i = worksheet.Dimension.Start.Row; i <= worksheet.Dimension.End.Row; i++)
                {
                    //loop all columns in a row
                    for (int j = worksheet.Dimension.Start.Column; j <= worksheet.Dimension.End.Column; j++)
                    {
                        //add the cell data to the List
                        if (worksheet.Cells[i, j].Value != null)
                        {
                            excelData.Add(worksheet.Cells[i, j].Value.ToString());
                        }
                    }
                }
            }
        }
    }

## Create a DataTable from Excel File
    public static DataTable ExcelPackageToDataTable(ExcelPackage excelPackage)
    {
        DataTable dt = new DataTable();
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets[1];

        //check if the worksheet is completely empty
        if (worksheet.Dimension == null)
        {
            return dt;
        }

        //create a list to hold the column names
        List<string> columnNames = new List<string>();

        //needed to keep track of empty column headers
        int currentColumn = 1;

        //loop all columns in the sheet and add them to the datatable
        foreach (var cell in worksheet.Cells[1, 1, 1, worksheet.Dimension.End.Column])
        {
            string columnName = cell.Text.Trim();

            //check if the previous header was empty and add it if it was
            if (cell.Start.Column != currentColumn)
            {
                columnNames.Add("Header_" + currentColumn);
                dt.Columns.Add("Header_" + currentColumn);
                currentColumn++;
            }

            //add the column name to the list to count the duplicates
            columnNames.Add(columnName);

            //count the duplicate column names and make them unique to avoid the exception
            //A column named 'Name' already belongs to this DataTable
            int occurrences = columnNames.Count(x => x.Equals(columnName));
            if (occurrences > 1)
            {
                columnName = columnName + "_" + occurrences;
            }

            //add the column to the datatable
            dt.Columns.Add(columnName);

            currentColumn++;
        }

        //start adding the contents of the excel file to the datatable
        for (int i = 2; i <= worksheet.Dimension.End.Row; i++)
        {
            var row = worksheet.Cells[i, 1, i, worksheet.Dimension.End.Column];
            DataRow newRow = dt.NewRow();

            //loop all cells in the row
            foreach (var cell in row)
            {
                newRow[cell.Start.Column - 1] = cell.Text;
            }

            dt.Rows.Add(newRow);
        }

        return dt;
    }

