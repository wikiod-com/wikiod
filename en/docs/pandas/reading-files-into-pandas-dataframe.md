---
title: "Reading files into pandas DataFrame"
slug: "reading-files-into-pandas-dataframe"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Read table into DataFrame
## Table file with header, footer, row names, and index column:
**file: table.txt**

    This is a header that discusses the table file
    to show space in a generic table file

    index  name     occupation
    1      Alice    Salesman
    2      Bob      Engineer
    3      Charlie  Janitor  

    This is a footer because your boss does not understand data files

**code:**

    import pandas as pd
    # index_col=0 tells pandas that column 0 is the index and not data 
    pd.read_table('table.txt', delim_whitespace=True, skiprows=3, skipfooter=2, index_col=0)

**output:**

              name occupation
    index
    1        Alice   Salesman
    2          Bob   Engineer
    3      Charlie    Janitor



## Table file without row names or index:
**file: table.txt**

    Alice    Salesman
    Bob      Engineer
    Charlie  Janitor 

**code:**

    import pandas as pd 
    pd.read_table('table.txt', delim_whitespace=True, names=['name','occupation'])

**output:**

          name occupation
    0    Alice   Salesman
    1      Bob   Engineer
    2  Charlie    Janitor

All options can be found in the pandas documentation [here](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.read_table.html)


## Read CSV File
## Data with header, separated by semicolons instead of commas

**file: table.csv** 

    index;name;occupation
    1;Alice;Saleswoman
    2;Bob;Engineer
    3;Charlie;Janitor

**code:**

    import pandas as pd
    pd.read_csv('table.csv', sep=';', index_col=0)

**output**:

              name occupation
    index
    1        Alice   Salesman
    2          Bob   Engineer
    3      Charlie    Janitor


## Table without row names or index and commas as separators

**file: table.csv**

    Alice,Saleswoman
    Bob,Engineer
    Charlie,Janitor

**code:**

    import pandas as pd 
    pd.read_csv('table.csv', names=['name','occupation'])

**output:**

          name occupation
    0    Alice   Salesman
    1      Bob   Engineer
    2  Charlie    Janitor


further clarification can be found in the [`read_csv`][1] documentation page


  [1]: http://pandas.pydata.org/pandas-docs/stable/generated/pandas.read_csv.html

## Collect google spreadsheet data into pandas dataframe
Sometimes we need to collect data from google spreadsheets. We can use **gspread** and **oauth2client** libraries to collect data from google spreadsheets. Here is a example to collect data:

**Code:**

    from __future__ import print_function
    import gspread
    from oauth2client.client import SignedJwtAssertionCredentials
    import pandas as pd
    import json
    
    scope = ['https://spreadsheets.google.com/feeds']
    
    credentials = ServiceAccountCredentials.from_json_keyfile_name('your-authorization-file.json', scope)
    
    gc = gspread.authorize(credentials)
    
    work_sheet = gc.open_by_key("spreadsheet-key-here")
    sheet = work_sheet.sheet1
    data = pd.DataFrame(sheet.get_all_records()) 
    
    print(data.head())

