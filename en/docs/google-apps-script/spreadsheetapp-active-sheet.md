---
title: "SpreadsheetApp Active Sheet"
slug: "spreadsheetapp-active-sheet"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Method:
getActive()

Return Type:
[Spreadsheet][1]


  [1]: https://developers.google.com/apps-script/reference/spreadsheet/spreadsheet

## getActive() - Get active spreadsheet
This returns the currently active spreadsheet, or null if there is none.

    var currentSheet = SpreadsheetApp.getActive();
    var url = currentSheet.getUrl();
    Logger.log( url );

