---
title: "Interaction with Excel"
slug: "interaction-with-excel"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Simple writing to Excel
A basic script showing the fundamentals of how to create/open, write to, and save an Excel spreadsheet.  Using the AutoIt Excel library functions.

    ; Include definitions of the User Excel functions
    #include <Excel.au3>
    
    ; Include Message Box codes for the error messages
    #include <MsgBoxConstants.au3>
    
    ; Constants
    $xlforce   = True            ; True = force new instance, False = use existing
    $xlkeybrd  = True            ; True = allow keys,           False = block keys
    $xlscreen  = True            ; True = live screen,          False = suppressed
    $xlvisible = True            ; True = visible,                False = invisible
    
    ; Variables
    $book  = 0                    ; The workbook
    $excel = 0                    ; The instance of Excel
    $range = 0                    ; Range object within a sheet
    
    ; Create an instance of Excel and attach this script to it
    ; Use the constants defined above to control visibility of the Excel window
    $excel = _Excel_Open($xlvisible, False, $xlvisible, $xlkeybrd, $xlforce)
    If ($excel = 0) Then HandleError("Excel creation failed ")
    
    ; Create a new workbook with one worksheet within the opened instance
    $book = _Excel_BookNew($excel, 1)
    If ($book = 0) Then HandleError("Workbook creation failed ")
    
    ; Example write to a single cell in the current sheet - writes 42 to cell B2
    $range = _Excel_RangeWrite($book, Default, 42, "B2")
    If ($range = 0) Then HandleError("Write cell failed ")
    
    ; Example write to a block in the current sheet - writes to cells D4:E5
    Local $xy[2][2] = [[1, 2], [3, 4]]
    $range = _Excel_RangeWrite($book, Default, $xy, "D4:E5")
    If ($range = 0) Then HandleError("Write cell failed ")
    
    ; Save the created workbook - without a given path this will go to the temporary
    ; directory.  The argument 'TRUE' forces an overwrite of any existing file
    $err = _Excel_BookSaveAs($book, "test.xlsx", $xlWorkbookDefault, True)
    If ($err = 0) Then HandleError("Workbook save failed ")
    
    ; Close the workbook.  The argument 'TRUE' will save any changes
    $err = _Excel_BookClose($book, True)
    If ($err = 0) Then HandleError("Workbook close failed ")
    
    ; Close the created instance of Excel
    ;     First argument 'TRUE' saves changes to any open worksheets
    ;    Second argument 'FALSE' prevents closure if the instance was not new at _Excel_Open()
    $err = _Excel_Close($excel, True, False)
    If ($err = 0) Then HandleError("Excel close failed ")
    
    Exit                                    ; End of script
    
    
    ; Error handler function - very simple, just aborts
    Func HandleError($message)
       MsgBox($MB_SYSTEMMODAL + $MB_ICONERROR, "Excel Test", $message & @error, 5)
       Exit
    EndFunc



