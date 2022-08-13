---
title: "NPOI Data validation constraint approach for Date, Time , List Item , email etc. for XSSF(.xslx) excel file using c#"
slug: "npoi-data-validation-constraint-approach-for-date-time--list-item--email-etc-for-xssfxslx-excel-file-using-c"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Creating the data validation constraint can be tricky and time taking in NPOI. I have shared some of my workable approach. These approach will give good idea to customize your own constraint types.

## Set the Date Constraints for Date Field Values Between 01/01/1900 To 12/31/2119 with Date Format mm/dd//yyyyy;
      int DVRowLimit = (Int16.MaxValue); 
            CellRangeAddressList cellRangeFieldsType1 = new CellRangeAddressList(1, DVRowLimit, targetFirstCol, targetLastCol);
            XSSFDataValidationConstraint dvConstraint = (XSSFDataValidationConstraint)validationHelper.CreateDateConstraint(OperatorType.BETWEEN, "=DATE(1900,1,1)", "=DATE(2119,12,31)", "mm/dd/yyyyy");
        //dvConstraint = (XSSFDataValidationConstraint)validationHelper.CreateDateConstraint(OperatorType.IGNORED, "", "", "m/d/yy h:mm");
            XSSFDataValidation dataValidation = (XSSFDataValidation)validationHelper.CreateValidation(dvConstraint, cellRangeFieldsType1);
            dataValidation.ShowErrorBox = true;
            dataValidation.ErrorStyle = 0;
            dataValidation.CreateErrorBox("InvalidDate", "Allowed Format is MM/DD/YYYY");
            dataValidation.ShowErrorBox = true;
            dataValidation.CreatePromptBox("Date Data Validation", "Enter Date in Format MM/DD/YYYY.");
            dataValidation.ShowPromptBox = true;
            sheet.AddValidationData(dataValidation);

## Set the Time Constraints for Field Values Between 00:00 To 23:59 with Date Format HH:MM;
      dvConstraint = (XSSFDataValidationConstraint)validationHelper.CreateTimeConstraint(OperatorType.BETWEEN, "=TIME(0,0,0)", "=TIME(23,59,59)");

## Create a list item Constraint
     dvConstraint = (XSSFDataValidationConstraint)validationHelper.CreateExplicitListConstraint(new string[] { "MON", "TUE" , "WED", "THU", "FRI"});

## Phone Number Constraint
//try same approach for currency types with format like "$#,###.00"and date "m/d/yy h:mm"

    XSSFFont defaultFont = (XSSFFont)workbook.CreateFont();
    defaultFont.FontHeightInPoints = (short)10;
    defaultFont.FontName = "Arial";
    XSSFCellStyle phoneCellStyle = (XSSFCellStyle)workbook.CreateCellStyle();
    XSSFDataFormat phoneDataFormat = (XSSFDataFormat)workbook.CreateDataFormat();
    phoneCellStyle.SetDataFormat(phoneDataFormat.GetFormat("000-000-0000"));
    phoneCellStyle.FillBackgroundColor = IndexedColors.LightYellow.Index;
    dvConstraint = (XSSFDataValidationConstraint)validationHelper.CreateintConstraint(OperatorType.BETWEEN, "1000000000", "9999999999");
    sheet.AddValidationData(dataValidation);
    sheet.SetDefaultColumnStyle(headerCount, phoneCellStyle);


## Email Address constraint For Email Columns
     string emailValidationFormula = GetEmailValidationFormula(targetFirstCol);
     CellRangeAddressList cellRangeFieldsType5 = new CellRangeAddressList(1, DVRowLimit, targetFirstCol, targetLastCol);
         dvConstraint = (XSSFDataValidationConstraint)validationHelper.CreateCustomConstraint(emailValidationFormula)
  
// get the email address validation pattern
  private string GetEmailValidationFormula(int targetColumn)
    {

        int div = headerCount + 1;
        string colLetter = String.Empty;
        int mod = 0;

        try
        {
            while (div > 0)
            {
                mod = (div - 1) % 26;
                colLetter = (char)(65 + mod) + colLetter;
                div = (int)((div - mod) / 26);
            }
        }
        catch (Exception Ex)
        {
            logger.Error("Error ", Ex);
        }

        return "=AND(FIND(\"@\"," + colLetter + "2),FIND(\".\"," + colLetter + "2),ISERROR(FIND(\" \"," + colLetter + "2)))";

    }



