---
title: "User Input Validation"
slug: "user-input-validation"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

How to validade user inputs. Validation constrains the values a user can input into a cell, and/or set a combobox for the user select the value for the cell. Optionally, a message can be displayed when the user clicks in a cell and a message error, when the validation fails. 

## List Validation
<!-- language-all: lang-c# -->
    //Add a List validation to B column. Values should be in a list
    var val = worksheet.DataValidations.AddListValidation("B:B");
    //Shows error message when the input doesn't match the accepted values
    val.ShowErrorMessage = true;
    //Style of warning. "information" and "warning" allow users to ignore the validation,
    //while "stop" and "undefined" doesn't
    val.ErrorStyle = OfficeOpenXml.DataValidation.ExcelDataValidationWarningStyle.information;
    //Title of the error mesage box
    val.ErrorTitle = "This is the title";
    //Message of the error
    val.Error = "This is the message";
    //Set to true to show a prompt when user clics on the cell
    val.ShowInputMessage = true;
    //Set the message for the prompt
    val.Prompt = "This is a input message";
    //Set the title for the prompt
    val.PromptTitle = "This is the title from the input message";
    //Define the accepted values
    val.Formula.Values.Add("This is accepted");
    val.Formula.Values.Add("This is also accepted");
    val.Formula.Values.Add("Any other thing is rejected");
    //Set to true if blank value is accepted
    val.AllowBlank = false;

    //Add a List validation to the C column
    var val2 = worksheet.DataValidations.AddListValidation("C:C");
    //Define the Cells with the accepted values
    val2.Formula.ExcelFormula = "=$D$3:$D$5";
    //Fill the cells with the accepted values
    worksheet.Cells["D3"].Value = "Val1";
    worksheet.Cells["D4"].Value = "Val2";
    worksheet.Cells["D5"].Value = "Val3";

## Integer Validation
<!-- language-all: lang-c# -->
    //Add a List validation to the C column
    var val3 = worksheet.DataValidations.AddIntegerValidation("E:E");
    //For Integer Validation, you have to set error message to true
    val3.ShowErrorMessage = true;
    val3.Error = "The value must be an integer between 0 and 10";
    //Minimum allowed Value
    val3.Formula.Value = 0;
    //Maximum allowed Value
    val3.Formula2.Value = 10;
    //If the cells are not filled, allow blanks or fill with a valid value, 
    //otherwise it could generate a error when saving 
    val3.AllowBlank = true;

## DateTime Validation
<!-- language-all: lang-c# -->
    //Add a DateTime Validation to column F
    var val4 = worksheet.DataValidations.AddDateTimeValidation("F:F");
    //For DateTime Validation, you have to set error message to true
    val4.ShowErrorMessage = true;
    //Minimum allowed date
    val4.Formula.Value = new DateTime(2017,03,15, 01, 0,0);
    //Maximum allowed date
    val4.Formula2.Value= new DateTime(2017, 03, 16, 12, 0, 0);
    val4.AllowBlank = true;

## Text Length Validation
<!-- language-all: lang-c# -->
    //Add a TextLength Validation to column G
    var val5 = worksheet.DataValidations.AddTextLengthValidation("G:G");
    //For TextLenght Validation, you have to set error message to true
    val5.ShowErrorMessage = true;
    //Minimum allowed text lenght
    val5.Formula.Value = 3;
    //Maximum allowed text lenght
    val5.Formula2.Value = 5;
    val5.AllowBlank = true;

