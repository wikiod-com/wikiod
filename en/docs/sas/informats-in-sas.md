---
title: "Informats in SAS"
slug: "informats-in-sas"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

SAS `informats` instruct SAS on how to read data from any input location (such as a file, an excel spreadsheet, a named pipe, or even another SAS variable, etc.) into a variable. 

SAS has just two data types - character and numeric, and each informat is specific to storing the value into either a character or numeric variable.  If the destination variable is a character, then the informat will begin with a `$` symbol, anything else will be a numeric informat.

Informats are very important especially when we import data from other datasets. For example, most of the times while working on real time data, we extract data from various data sources (Oracle,Mysql,Teradata etc). Every time we import data we need to specify the informat statement so SAS can read the data properly.

## Importing excel data into SAS
For example, say below is the sample data in an Excel 'Test',

    Purchase_Date    Customer_Name    Price
    05-05-2017    Adam    1075
    06-05-2017    Noah    1093
    07-05-2017    Peter    1072
    08-05-2017    Louis    1101
    09-05-2017    Zoe    1248
    10-05-2017    Kevin    1045
    11-05-2017    Messiah    1072
    12-05-2017    John    1046
    13-05-2017    Stephen    1043
    14-05-2017    Solly    1113
    15-05-2017    Jeevan    1137

You should use the below code to import this successfully,

    Data Test;
    Infile 'D:\Test.csv';
    Delimiter=',' Missover DSD Getnames=Yes;
    Informat Purchase_Date date9.;
    Informat Price dollarx10.2;
    Format Purchase_Date date9.;
    Format Price dollarx10.2;
    run;


>     Informat in the above code helps SAS to read the data from Excel.
>     Format in the above code helps to write the data properly into SAS Data set.

## Importing character vs numeric
The example below uses the input statement to read a value from a source (in this case the string `123`) into a both a character destination and a numeric destination.

    data test;
       source = '123';
       numeric_destination = input(source, best.);
       character_destination = input(source, $3.);
    run;

