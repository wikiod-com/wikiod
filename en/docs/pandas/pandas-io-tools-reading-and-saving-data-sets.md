---
title: "Pandas IO tools (reading and saving data sets)"
slug: "pandas-io-tools-reading-and-saving-data-sets"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

The pandas official documentation includes a page on [IO Tools](http://pandas.pydata.org/pandas-docs/stable/io.html) with a list of relevant functions to read and write to files, as well as some examples and common parameters.

## Reading csv file into DataFrame
Example for reading file `data_file.csv` such as:

## File:

    index,header1,header2,header3
    1,str_data,12,1.4
    3,str_data,22,42.33
    4,str_data,2,3.44
    2,str_data,43,43.34
    
    7, str_data, 25, 23.32

## Code:

    pd.read_csv('data_file.csv')
    

## Output:

       index    header1  header2  header3
    0      1   str_data       12     1.40
    1      3   str_data       22    42.33
    2      4   str_data        2     3.44
    3      2   str_data       43    43.34
    4      7   str_data       25    23.32

## Some useful arguments:

 - **`sep`**
    The default field delimiter is a comma `,`. Use this option if you need a different delimiter, for instance `pd.read_csv('data_file.csv', sep=';')`
 - **`index_col`** With `index_col = n` (`n` an integer) you tell pandas to use column `n` to index the DataFrame. In the above example:

       pd.read_csv('data_file.csv',  index_col=0)

    Output:

                 header1  header2  header3
       index
        1       str_data       12     1.40
        3       str_data       22    42.33
        4       str_data        2     3.44
        2       str_data       43    43.34
        7       str_data       25    23.32


   - **`skip_blank_lines`** By default blank lines are skipped. Use `skip_blank_lines=False` to include blank lines (they will be filled with `NaN` values)

         pd.read_csv('data_file.csv',  index_col=0,skip_blank_lines=False)

     Output:

                  header1  header2  header3
         index
          1      str_data       12     1.40
          3      str_data       22    42.33
          4      str_data        2     3.44
          2      str_data       43    43.34
         NaN          NaN      NaN      NaN
          7      str_data       25    23.32

   - **`parse_dates`** Use this option to parse date data.

      File:

         date_begin;date_end;header3;header4;header5
         1/1/2017;1/10/2017;str_data;1001;123,45
         2/1/2017;2/10/2017;str_data;1001;67,89
         3/1/2017;3/10/2017;str_data;1001;0

      Code to parse columns `0` and `1` as dates:

         pd.read_csv('f.csv', sep=';', parse_dates=[0,1])

      Output:

           date_begin   date_end   header3  header4 header5
         0 2017-01-01 2017-01-10  str_data     1001  123,45
         1 2017-02-01 2017-02-10  str_data     1001   67,89
         2 2017-03-01 2017-03-10  str_data     1001       0

      By default, the date format is inferred. If you want to specify a date format you can use for instance

         dateparse = lambda x: pd.datetime.strptime(x, '%d/%m/%Y')
         pd.read_csv('f.csv', sep=';',parse_dates=[0,1],date_parser=dateparse)

      Output:

           date_begin   date_end   header3  header4 header5
         0 2017-01-01 2017-10-01  str_data     1001  123,45
         1 2017-01-02 2017-10-02  str_data     1001   67,89
         2 2017-01-03 2017-10-03  str_data     1001       0   


More information on the function's parameters can be found in the [official documentation][1].

  [1]: http://pandas.pydata.org/pandas-docs/stable/generated/pandas.read_csv.html


## Read a specific sheet


## Parsing dates when reading from csv


## Spreadsheet to dict of DataFrames


## List comprehension


## Read in chunks
    import pandas as pd    
    
    chunksize = [n]
    for chunk in pd.read_csv(filename, chunksize=chunksize):
        process(chunk)
        delete(chunk)



## Save to CSV file
**Save with default parameters:**
    
    df.to_csv(file_name)

**Write specific columns:**

    df.to_csv(file_name, columns =['col'])


----------


**Difault delimiter is ',' - to change it:**

    df.to_csv(file_name,sep="|")


----------


**Write without the header:**

    df.to_csv(file_name, header=False)


----------


**Write with a given header:**

    df.to_csv(file_name, header = ['A','B','C',...]


----------


**To use a specific encoding (e.g. 'utf-8') use the encoding argument:**

   df.to_csv(file_name, encoding='utf-8')



## Parsing date columns with read_csv


## Read Nginx access log (multiple quotechars)
For multiple quotechars use regex in place of sep:

    df = pd.read_csv(log_file,
                  sep=r'\s(?=(?:[^"]*"[^"]*")*[^"]*$)(?![^\[]*\])',
                  engine='python',
                  usecols=[0, 3, 4, 5, 6, 7, 8],
                  names=['ip', 'time', 'request', 'status', 'size', 'referer', 'user_agent'],
                  na_values='-',
                  header=None
                    )

## Basic saving to a csv file
    raw_data = {'first_name': ['John', 'Jane', 'Jim'],
                'last_name': ['Doe', 'Smith', 'Jones'],
                'department': ['Accounting', 'Sales', 'Engineering'],}
    df = pd.DataFrame(raw_data,columns=raw_data.keys())
    df.to_csv('data_file.csv')

## Testing read_csv


## Read & merge multiple CSV files (with the same structure) into one DF


## Reading cvs file into a pandas data frame when there is no header row


## Using HDFStore
    import string
    import numpy as np
    import pandas as pd

# generate sample DF with various dtypes

    df = pd.DataFrame({
         'int32':    np.random.randint(0, 10**6, 10),
         'int64':    np.random.randint(10**7, 10**9, 10).astype(np.int64)*10,
         'float':    np.random.rand(10),
         'string':   np.random.choice([c*10 for c in string.ascii_uppercase], 10),
         })
    
    In [71]: df
    Out[71]:
          float   int32       int64      string
    0  0.649978  848354  5269162190  DDDDDDDDDD
    1  0.346963  490266  6897476700  OOOOOOOOOO
    2  0.035069  756373  6711566750  ZZZZZZZZZZ
    3  0.066692  957474  9085243570  FFFFFFFFFF
    4  0.679182  665894  3750794810  MMMMMMMMMM
    5  0.861914  630527  6567684430  TTTTTTTTTT
    6  0.697691  825704  8005182860  FFFFFFFFFF
    7  0.474501  942131  4099797720  QQQQQQQQQQ
    8  0.645817  951055  8065980030  VVVVVVVVVV
    9  0.083500  349709  7417288920  EEEEEEEEEE

# make a bigger DF (10 * 100.000 = 1.000.000 rows)

    df = pd.concat([df] * 10**5, ignore_index=True)

# create (or open existing) HDFStore file

    store = pd.HDFStore('d:/temp/example.h5')

# save our data frame into `h5` (HDFStore) file, indexing [int32, int64, string] columns:

    store.append('store_key', df, data_columns=['int32','int64','string'])

# show HDFStore details

    In [78]: store.get_storer('store_key').table
    Out[78]:
    /store_key/table (Table(10,)) ''
      description := {
      "index": Int64Col(shape=(), dflt=0, pos=0),
      "values_block_0": Float64Col(shape=(1,), dflt=0.0, pos=1),
      "int32": Int32Col(shape=(), dflt=0, pos=2),
      "int64": Int64Col(shape=(), dflt=0, pos=3),
      "string": StringCol(itemsize=10, shape=(), dflt=b'', pos=4)}
      byteorder := 'little'
      chunkshape := (1724,)
      autoindex := True
      colindexes := {
        "index": Index(6, medium, shuffle, zlib(1)).is_csi=False,
        "int32": Index(6, medium, shuffle, zlib(1)).is_csi=False,
        "string": Index(6, medium, shuffle, zlib(1)).is_csi=False,
        "int64": Index(6, medium, shuffle, zlib(1)).is_csi=False}

# show indexed columns

    In [80]: store.get_storer('store_key').table.colindexes
    Out[80]:
    {
        "int32": Index(6, medium, shuffle, zlib(1)).is_csi=False,
        "index": Index(6, medium, shuffle, zlib(1)).is_csi=False,
        "string": Index(6, medium, shuffle, zlib(1)).is_csi=False,
        "int64": Index(6, medium, shuffle, zlib(1)).is_csi=False}

# close (flush to disk) our store file

    store.close()

