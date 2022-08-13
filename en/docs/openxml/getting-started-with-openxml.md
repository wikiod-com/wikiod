---
title: "Getting started with openxml"
slug: "getting-started-with-openxml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Create a new Spreadsheet with OpenXML
This method will create a new Excel Spreadsheet.  Pass in the `fileName` which is a full file path name.

    using DocumentFormat.OpenXml;
    using DocumentFormat.OpenXml.Packaging;
    using DocumentFormat.OpenXml.Spreadsheet;
    using System;
    ....
        void Create(string fileName)
        {

            using (SpreadsheetDocument document = SpreadsheetDocument.Create(fileName, SpreadsheetDocumentType.Workbook))
            {
                var relationshipId = "rId1";

                //build Workbook Part
                var workbookPart = document.AddWorkbookPart();
                var workbook = new Workbook();
                var sheets = new Sheets();
                var sheet1 = new Sheet() { Name = "First Sheet", SheetId = 1, Id = relationshipId };
                sheets.Append(sheet1);
                workbook.Append(sheets);
                workbookPart.Workbook = workbook;

                //build Worksheet Part
                var workSheetPart = workbookPart.AddNewPart<WorksheetPart>(relationshipId);
                var workSheet = new Worksheet();
                workSheet.Append(new SheetData());
                workSheetPart.Worksheet = workSheet;
        
                //add document properties
                document.PackageProperties.Creator = "Your Name";
                document.PackageProperties.Created = DateTime.UtcNow;

            }

For this project, make sure you include the reference to `DocumentFormat.OpenXml`.  This is located in the path specified in the Installing OpenXML Example. 

The spreadsheet will be created with **Your Name** as the Author and the first Worksheet named **First Sheet**.

[![enter image description here][1]][1]

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/O4irw.png
  [2]: https://i.stack.imgur.com/eDBE0.png

## Using Open XML SDK 2.5 Productivity Tool
Reading the specification for the document formats in OpenXML can be a time consuming process. Sometimes you just want to see how to produce a certain feature in a word-document.
The Open XML SDK 2.5 Productivity Tool for Microsoft Office (OpenXmlSdkTool.exe) does just that. Its main features are:
- See the structure of a file - which xml-parts does it contain
- Navigate the xml in each of these parts
- Generate c#-code for producing the selected part of the document
- Link to the file format specification describing more details
- Document OpenXML Validation

For a simple 'Hello world.docx' it looks like this:
[![enter image description here][1]][1]
The pane on the left show the document-structure. The top-right pane displays the xml corresponding to the selection in the tree, and finally the bottom-right pane show some generated code for producing the xml displayed above it.

This enables a very hands on way to investigate a certain feature:
- Produce an example-document (fx a word-document)
- Open the document in Productivity Tool
- Use 'Reflect Code' to generate code

The SDK can be downloaded from https://www.microsoft.com/en-us/download/details.aspx?id=30425 - download and install both of the msi packages. After installation use OpenXMLSdkTool.exe installed in "C:\Program Files (x86)\Open XML SDK\V2.5\tool".


  [1]: https://i.stack.imgur.com/Sfc6y.png

## Installation of OpenXML SDK and productivity tool on your computer
Go to the [Microsoft link for the OpenXML SDK][1] download.  Click the red download button.  On the next screen click the box next to OpenXMLSDKToolV25.msi and click next to begin the download.

Once the download is complete, launch the OpenXMLSDKToolV25.msi and follow the instructions on the screen.

The installer places the files in the following default directory:

`"C:\Program Files (x86)\Open XML SDK\V2.5"`

In this directory is a readme that explains how to use the SDK and a readme for the productivity tool.

  [1]: https://www.microsoft.com/en-us/download/details.aspx?id=30425

