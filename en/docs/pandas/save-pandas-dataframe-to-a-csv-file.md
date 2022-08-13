---
title: "Save pandas dataframe to a csv file"
slug: "save-pandas-dataframe-to-a-csv-file"
draft: false
images: []
weight: 5241
type: docs
toc: true
---

## Parameters


| Parameter | Description |
| ------ | ------ |
|path_or_buf | string or file handle, default None File path or object, if None is provided the result is returned as a string.|
|sep | character, default ‘,’ Field delimiter for the output file.|
|na_rep | string, default ‘’ Missing data representation |
|float_format | string, default None Format string for floating point numbers|
|columns | sequence, optional Columns to write|
|header | boolean or list of string, default True Write out column names. If a list of string is given it is assumed to be aliases for the column names|
|index | boolean, default True Write row names (index)|
|index_label | string or sequence, or False, default None Column label for index column(s) if desired. If None is given, and header and index are True, then the index names are used. A sequence should be given if the DataFrame uses MultiIndex. If False do not print fields for index names. Use index_label=False for easier importing in R| 
|nanRep | None deprecated, use na_rep|
|mode | str Python write mode, default ‘w’|
|encoding | string, optional A string representing the encoding to use in the output file, defaults to ‘ascii’ on Python 2 and ‘utf-8’ on Python 3.|
|compression | string, optional a string representing the compression to use in the output file, allowed values are ‘gzip’, ‘bz2’, ‘xz’, only used when the first argument is a filename|
|line_terminator | string, default ‘n’ The newline character or character sequence to use in the output file|
|quoting | optional constant from csv module defaults to csv.QUOTE_MINIMAL|
|quotechar | string (length 1), default ‘”’ character used to quote fields |
|doublequote | boolean, default True Control quoting of quotechar inside a field|
|escapechar | string (length 1), default None character used to escape sep and quotechar when appropriate|
|chunksize | int or None rows to write at a time|
|tupleize_cols | boolean, default False write multi_index columns as a list of tuples (if True) or new (expanded format) if False)|
|date_format | string, default None Format string for datetime objects|
|decimal | string, default ‘.’ Character recognized as decimal separator. E.g. use ‘,’ for European data|

## Create random DataFrame and write to .csv
Create a simple DataFrame.

    import numpy as np
    import pandas as pd

    # Set the seed so that the numbers can be reproduced.
    np.random.seed(0)  

    df = pd.DataFrame(np.random.randn(5, 3), columns=list('ABC'))
    
    # Another way to set column names is "columns=['column_1_name','column_2_name','column_3_name']"

    df

          A         B         C
    0  1.764052  0.400157  0.978738
    1  2.240893  1.867558 -0.977278
    2  0.950088 -0.151357 -0.103219
    3  0.410599  0.144044  1.454274
    4  0.761038  0.121675  0.443863

Now, write to a CSV file:

    df.to_csv('example.csv', index=False)

Contents of example.csv:
    
    A,B,C
    1.76405234597,0.400157208367,0.978737984106
    2.2408931992,1.86755799015,-0.977277879876
    0.950088417526,-0.151357208298,-0.103218851794
    0.410598501938,0.144043571161,1.45427350696
    0.761037725147,0.121675016493,0.443863232745

Note that we specify `index=False` so that the auto-generated indices (row #s 0,1,2,3,4) are not included in the CSV file. Include it if you need the index column, like so:

    df.to_csv('example.csv', index=True)  # Or just leave off the index param; default is True

Contents of example.csv:

    ,A,B,C
    0,1.76405234597,0.400157208367,0.978737984106
    1,2.2408931992,1.86755799015,-0.977277879876
    2,0.950088417526,-0.151357208298,-0.103218851794
    3,0.410598501938,0.144043571161,1.45427350696
    4,0.761037725147,0.121675016493,0.443863232745

Also note that you can remove the header if it's not needed with `header=False`. This is the simplest output:

    df.to_csv('example.csv', index=False, header=False)

Contents of example.csv:

    1.76405234597,0.400157208367,0.978737984106
    2.2408931992,1.86755799015,-0.977277879876
    0.950088417526,-0.151357208298,-0.103218851794
    0.410598501938,0.144043571161,1.45427350696
    0.761037725147,0.121675016493,0.443863232745

The delimiter can be set by `sep=` argument, although the standard separator for csv files is `','` .

`df.to_csv('example.csv', index=False, header=False, sep='\t')`

    1.76405234597    0.400157208367    0.978737984106
    2.2408931992    1.86755799015    -0.977277879876
    0.950088417526    -0.151357208298    -0.103218851794
    0.410598501938    0.144043571161    1.45427350696
    0.761037725147    0.121675016493    0.443863232745


## Save Pandas DataFrame from list to dicts to csv with no index and with data encoding


    import pandas as pd
    data = [
        {'name': 'Daniel', 'country': 'Uganda'},
        {'name': 'Yao', 'country': 'China'},
        {'name': 'James', 'country': 'Colombia'},
    ]
    df = pd.DataFrame(data)
    filename = 'people.csv'
    df.to_csv(filename, index=False, encoding='utf-8')

