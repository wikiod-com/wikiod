---
title: "Spreadsheet Add Menu"
slug: "spreadsheet-add-menu"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
 1. addMenu(name, subMenus)

## Parameters
| Name      | Description                       |
| ------    | --------------------------------- |
| name      | the name of the menu to be created|
| subMenus  | an array of JavaScript maps       |

Usually, you will want to call addMenu from the onOpen function so that the menu is automatically created when the Spreadsheet is loaded.

     // The onOpen function is executed automatically every time a Spreadsheet is loaded
     function onOpen() {
       var activeSheet = SpreadsheetApp.getActiveSpreadsheet();
       var menuItems = [];
       // When the user clicks on "addMenuExample" then "Menu 1", the function Myfunction1 is executed.
       menuItems.push({name: "Menu 1", functionName: "Myfunction1"});
       menuItems.push(null); // adding line separator
       menuItems.push({name: "Menu 2", functionName: "Myfunction2"});
    
       activeSheet.addMenu("addMenuExample", menuEntries);
     }

## Create Custom Menu
/*
*********************************************************************************
Method: To Create Custom Menu
This is First Function to be called when App Loads
*********************************************************************************
*/

    function onOpen() {
      var ui = SpreadsheetApp.getUi();
      // Or DocumentApp or FormApp.
      ui.createMenu('My HR')
          .addItem('Send Form to All', 'sendIDPForm_All')
          .addItem('Trigger IDP System', 'applyCategory')
          .addToUi();
    }

## Create a new menu
Creates a new menu in the Spreadsheet UI. Each menu entry runs a user-defined function. 

<!-- language: lang-js -->

       var activeSheet = SpreadsheetApp.getActiveSpreadsheet();
       var menuItems = [];
       // When the user clicks on "addMenuExample" then "Menu 1", the function Myfunction1 is executed.
       menuItems.push({name: "Menu 1", functionName: "Myfunction1"});
       menuItems.push(null); // adding line separator
       menuItems.push({name: "Menu 2", functionName: "Myfunction2"});
    
       activeSheet.addMenu("addMenuExample", menuEntries);

