---
title: "NPOI Data validation approach for XSSF(.xslx) excel file using c#"
slug: "npoi-data-validation-approach-for-xssfxslx-excel-file-using-c"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Data validation allows user to create a drop-down list and restrict values in the cell to these entries. Due to limitation Excel can't bind  more than 256 characters programmatically. To bind more than 256 characters one can follow explained approach.

## When Sum of all list item's total character count less than 256
You can read all items either from any config file or type it inline. 

Considering if its saved in Config File 
 

    // Read all list items from config file
    string[] countryDV = ConfigurationManager.AppSettings["countryDV"].Split(',').Select(s => s.Trim().ToUpper()).ToArray();
    int DVRowLimit = (Int16.MaxValue); 
    CellRangeAddressList countryDVAddList = new CellRangeAddressList(1, DVRowLimit, 0, 0);
    dvConstraint = (XSSFDataValidationConstraint)validationHelper.CreateExplicitListConstraint(countryDV);
    // In case of Inline list values 
    // use this approach:  dvConstraint = (XSSFDataValidationConstraint)validationHelper.CreateExplicitListConstraint(new string[] { "USA", "CANADA"});
    dataValidation = (XSSFDataValidation)validationHelper.CreateValidation(dvConstraint, countryDVAddList);
    dataValidation.ShowErrorBox = true;
    dataValidation.SuppressDropDownArrow = true;
    dataValidation.ErrorStyle = 0;
    dataValidation.CreateErrorBox("InvalidValue", "Select Valid country.");
    dataValidation.ShowErrorBox = true;
    dataValidation.CreatePromptBox("country Data Validation", "Enter country.");
    dataValidation.ShowPromptBox = true;
    sheet.AddValidationData(dataValidation);

## When Sum of all list item's total character count more than 256.
Approach in this case will be different than previous example because Excel file supports the Data validation for list of items with total character count less than 256 if all items are binded directly as in previous example. But in many situation list items can be longer than 256 characters and in that case direct binding will not work.

However, excel file supports more than 256 character of list item if it is referred from different column of same excel file. So as a workaround once can read all values from database or settings file and keep it hidden from current view into one of the distant column and data validation can read this hidden column through formula to create list item. Below code will show this approach by reading data values through setting file.

    // Read all list items from config file
    string[] countryDV = ConfigurationManager.AppSettings["countryDV"].Split(',').Select(s => s.Trim().ToUpper()).ToArray();
    // Get the column name where you want to hide the list items, assume distant  column is "ZZ"
    string countryDVDataCellColumn = ConfigurationManager.AppSettings["countryDVDataCellColumn"].Trim().ToString(); 
    
      
    int DVRowLimit = (Int16.MaxValue); 
    // Creating and Assigning Settings for Data Validation
    CreateDropDownUsingCellReference(workbook, countryDV, "CountryConstraint", countryDVDataCellColumn);
    CellRangeAddressList countryDVAddList = new CellRangeAddressList(1, DVRowLimit, targetFirstCol, targetLastCol);
    dvConstraint = (XSSFDataValidationConstraint)validationHelper.CreateFormulaListConstraint("=CountryConstraint");
    dvConstraint.Validate();
    dataValidation = (XSSFDataValidation)validationHelper.CreateValidation(dvConstraint, countryDVAddList);
    dataValidation.ShowErrorBox = true;
    dataValidation.SuppressDropDownArrow = true;
    dataValidation.ErrorStyle = 0;
    dataValidation.CreateErrorBox("InvalidValue", "Select Valid country.");
    dataValidation.ShowErrorBox = true;
    dataValidation.CreatePromptBox("country Data Validation", "Enter country.");
    dataValidation.ShowPromptBox = true;
    sheet.AddValidationData(dataValidation);
    
    
    
    private void CreateDropDownUsingCellReference(XSSFWorkbook wb, string[] csvListOfValues, string listName, string headerName)
    {
        int columnIndex = CellReference.ConvertColStringToIndex(headerName);
        try
        {
        XSSFName namedCell = (XSSFName)wb.CreateName();
        namedCell.NameName = listName;
        //Creating Cell and Assigning Values from CSVListOfValues;
        for (int i = 0; i < csvListOfValues.Length; i++)
        {
                var namedRow = wb.GetSheetAt(0).CreateRow(i + 1);
                namedRow.CreateCell(columnIndex).SetCellValue(csvListOfValues[i]);
        }
    
        //Assigning the Reference for sheet 0 With Cell Range, where list items iscopied 
        String reference = wb.GetSheetAt(0).SheetName + "!$" + headerName + "$2:$" + headerName + "$" + (csvListOfValues.Length + 1).ToString();
        namedCell.RefersToFormula = reference;
    
        //Hiding the Column  now;
        wb.GetSheetAt(0).SetColumnHidden(columnIndex, true);
        }
        catch (Exception Ex)
        { }
    }






