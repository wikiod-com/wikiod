---
title: "Spreadsheet Service"
slug: "spreadsheet-service"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

The official API reference for the Spreadsheet Service can be found at https://developers.google.com/apps-script/reference/spreadsheet/.

## Sheet
**Getting a reference to a named sheet tab**

<!-- language: lang-js -->

    var spread_sheet = SpreadsheetApp.getActiveSpreadsheet();//Get active spreadsheet
    var sheet_with_name_a = spread_sheet.getSheetByName("sheet_tab_name");

**Getting active sheet tab**

<!-- language: lang-js -->

    var spread_sheet = SpreadsheetApp.getActiveSpreadsheet();
    var active_sheet = spread_sheet.getActiveSheet();

**Insert Column**

<!-- language: lang-js -->

    var spread_sheet = SpreadsheetApp.getActiveSpreadsheet();
    var active_sheet = spread_sheet.getActiveSheet();  
    active_sheet.insertColumnAfter(1);  // This inserts a column after the first column position
    active_sheet.insertColumnBefore(1);  // This inserts a column in the first column position
    active_sheet.insertColumns(1);  // Shifts all columns by one
    active_sheet.insertColumns(1, 3);  // Shifts all columns by three
    active_sheet.insertColumnsAfter(1);  // This inserts a column in the second column position
    active_sheet.insertColumnsBefore(1, 5);  // This inserts five columns before the first column
    
**Insert row**

<!-- language: lang-js -->

    var spread_sheet = SpreadsheetApp.getActiveSpreadsheet();
    var active_sheet = spread_sheet.getActiveSheet();  
    active_sheet.insertRowAfter(1);  // This inserts a row after the first row position
    active_sheet.insertRowBefore(1);  // This inserts a row in the first row position
    active_sheet.insertRows(1);  // Shifts all rows by one
    active_sheet.insertRows(1, 3);  // Shifts all rows by three
    active_sheet.insertRowsAfter(1);  // This inserts a row in the second row position
    active_sheet.insertRowsBefore(1, 5);  // This inserts five rows before the first row


**Cell value**

<!-- language: lang-js -->

    var spread_sheet = SpreadsheetApp.getActiveSpreadsheet();
    var active_sheet = spread_sheet.getActiveSheet();
    var cell = range.getCell(1, 1);
    var cell_value = cell.getValue();
    cell.setValue(100);

**Copy cells**

<!-- language: lang-js -->

    var spread_sheet = SpreadsheetApp.getActiveSpreadsheet();
    var active_sheet = spread_sheet.getActiveSheet();
    var rangeToCopy = active_sheet.getRange(1, 1, sheet.getMaxRows(), 5);
    rangeToCopy.copyTo(sheet.getRange(1, 6));

**Formula**

<!-- language: lang-js -->

    var spread_sheet = SpreadsheetApp.getActiveSpreadsheet();
    var active_sheet = spread_sheet.getActiveSheet();
    var range = active_sheet.getRange("B5");
    var formula = range.getFormula()
    range.setFormula("=SUM(B3:B4)");

## Copy a value from one sheet to current sheet
Imagine that we have a separate Google spreadsheet, and we need to get the B2 cell value to cell D5 on your current sheet.

<!-- language: lang-js -->

    function copyValueandPaste()
    {  
        var source = SpreadsheetApp.openById('spread sheet id is here'); //Separate spreadsheet book
        var sourcesheet = source.getSheetByName('Sheet1'); //Sheet tab with source data
        var sourceCellValue = sourcesheet.getRange('B2').getValue(); // get B2 cell value
        
        var thisBook = SpreadsheetApp.getActive(); // Active spreadsheet book
        var thisSheet = thisBook.getSheetByName('Sheet1'); // Target sheet
        thisSheet.getRange('D5').setValue(sourceCellValue); //Set value to target sheet D5 cell
    }

You can find spreadsheet id from your URL.

## Get the last row in a single column
<!-- language: lang-js -->

    function lastRowForColumn(sheet, column){
      // Get the last row with data for the whole sheet.
      var numRows = sheet.getLastRow();
      
      // Get all data for the given column
      var data = sheet.getRange(1, column, numRows).getValues();
      
      // Iterate backwards and find first non empty cell
      for(var i = data.length - 1 ; i >= 0 ; i--){
        if (data[i][0] != null && data[i][0] != ""){
          return i + 1;
        }
      }
    }

## Inserting Arrays as Rows
Inserting a row at the bottom of a spreadsheet is easy:

```
var someSheet = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
someSheet.appendRow(["Frodo", "Baggins", "Hobbit", "The Shire", 33]);
```

Note this will add the row after the last *non-empty* row.

Inserting a row somewhere in the middle is a bit more work:

```
var someSheet = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];

var newRowIndex = 2;
var row = ["Gandalf", "?", "Wizard", "?", 2019];
someSheet.insertRowBefore(newRowIndex);
// getRange(row, col, numRows, numCols)
someSheet.getRange(newRowIndex, 1, 1, row.length).setValues([row]); // Note 2D array!
```

A lot of this useless code can be abstracted into a helper function:
```
function insertRowBefore(sheet, rowIndex, rowData) {
  sheet.insertRowBefore(rowIndex);
  sheet.getRange(rowIndex, 1, 1, rowData.length).setValues([rowData]);
}
```

Which reduces our example to just:
```
var someSheet = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
insertRowBefore(someSheet, 2, ["Gandalf", "?", "Wizard", "?", 2019]);

```

