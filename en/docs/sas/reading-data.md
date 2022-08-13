---
title: "Reading Data"
slug: "reading-data"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Reading data into a SAS dataset can be accomplished using multiple approaches including the `datalines` statement, from an external file using an `infile` statement in the data step, or reading data from an external file using `proc import`. In addition you can read in data from external sources that are odbc compliant (e.g. SQL databases) using the odbc drivers. 




## PROC IMPORT for Excel, importing a specific sheet
There will be times where you only want to import a specific sheet from an excel file with multiple sheets. To do that, we'll use "**SHEET=**". 


    PROC IMPORT 
        OUT= YourNewTable
        DATAFILE= "myfolder/excelfilename.xlsx" 
        DBMS=xlsx 
        REPLACE;
        SHEET="Sheet1";
        GETNAMES=YES;
    RUN;
>Also take note of the ability to specify whether or not the top row imported contains column names or not (**GETNAMES=YES** (or NO).

## Read text file with comma delimiter
    DATA table-name;
        INFILE "file-path/file-name.csv" dsd;
        INPUT Name $ City $ Age;
    RUN;

## Read data from excel file
    PROC IMPORT DATAFILE = "file-path/file-name.xlsx" OUT=data_set DBMS=XLSX REPLACE;

