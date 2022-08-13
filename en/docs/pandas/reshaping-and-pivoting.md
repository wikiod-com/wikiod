---
title: "Reshaping and pivoting"
slug: "reshaping-and-pivoting"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Pivoting with aggregating
    import pandas as pd
    import numpy as np

    df = pd.DataFrame({'Name':['Mary', 'Jon','Lucy', 'Jane', 'Sue', 'Mary', 'Lucy'],
                       'Age':[35, 37, 40, 29, 31, 26, 28],
                       'City':['Boston', 'Chicago', 'Los Angeles', 'Chicago', 'Boston', 'Boston', 'Chicago'],
                       'Position':['Manager','Manager','Manager','Programmer', 'Programmer','Manager','Manager'],
                        'Sex':['Female','Male','Female','Female', 'Female','Female','Female']},
                        columns=['Name','Position','City','Age','Sex'])
    
    print (df)
       Name    Position         City  Age  Sex
    0  Mary     Manager       Boston   35  Female
    1   Jon     Manager      Chicago   37  Male
    2  Lucy     Manager  Los Angeles   40  Female
    3  Jane  Programmer      Chicago   29  Female
    4   Sue  Programmer       Boston   31  Female
    5  Mary     Manager       Boston   26  Female
    6  Lucy     Manager      Chicago   28  Female

If use [`pivot`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.pivot.html), get error:

    print (df.pivot(index='Position', columns='City', values='Age'))

>ValueError: Index contains duplicate entries, cannot reshape


Use [`pivot_table`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.pivot_table.html) with aggregating function:

    #default aggfunc is np.mean
    print (df.pivot_table(index='Position', columns='City', values='Age'))
    City        Boston  Chicago  Los Angeles
    Position                                
    Manager       30.5     32.5         40.0
    Programmer    31.0     29.0          NaN
    
    print (df.pivot_table(index='Position', columns='City', values='Age', aggfunc=np.mean))
    City        Boston  Chicago  Los Angeles
    Position                                
    Manager       30.5     32.5         40.0
    Programmer    31.0     29.0          NaN

Another agg functions:

    print (df.pivot_table(index='Position', columns='City', values='Age', aggfunc=sum))
    City        Boston  Chicago  Los Angeles
    Position                                
    Manager       61.0     65.0         40.0
    Programmer    31.0     29.0          NaN
    
    #lost data !!!
    print (df.pivot_table(index='Position', columns='City', values='Age', aggfunc='first'))
    City        Boston  Chicago  Los Angeles
    Position                                
    Manager       35.0     37.0         40.0
    Programmer    31.0     29.0          NaN

If need aggregate by columns with `string` values:
    
    print (df.pivot_table(index='Position', columns='City', values='Name')) 

>DataError: No numeric types to aggregate

You can use these aggragating functions:

    print (df.pivot_table(index='Position', columns='City', values='Name', aggfunc='first')) 
    City       Boston Chicago Los Angeles
    Position                             
    Manager      Mary     Jon        Lucy
    Programmer    Sue    Jane        None
    
    print (df.pivot_table(index='Position', columns='City', values='Name', aggfunc='last')) 
    City       Boston Chicago Los Angeles
    Position                             
    Manager      Mary    Lucy        Lucy
    Programmer    Sue    Jane        None
    
    print (df.pivot_table(index='Position', columns='City', values='Name', aggfunc='sum')) 
    City          Boston  Chicago Los Angeles
    Position                                 
    Manager     MaryMary  JonLucy        Lucy
    Programmer       Sue     Jane        None
    
    print (df.pivot_table(index='Position', columns='City', values='Name', aggfunc=', '.join)) 
    City            Boston    Chicago Los Angeles
    Position                                     
    Manager     Mary, Mary  Jon, Lucy        Lucy
    Programmer         Sue       Jane        None
    
    print (df.pivot_table(index='Position', columns='City', values='Name', aggfunc=', '.join, fill_value='-')
             .reset_index()
             .rename_axis(None, axis=1))
         Position      Boston    Chicago Los Angeles
    0     Manager  Mary, Mary  Jon, Lucy        Lucy
    1  Programmer         Sue       Jane           -

The information regarding the *Sex* has yet not been used. It could be switched by one of the columns, or it could be added as another level:

    print (df.pivot_table(index='Position', columns=['City','Sex'], values='Age', aggfunc='first'))

    City       Boston Chicago       Los Angeles
    Sex        Female  Female  Male      Female
    Position
    Manager      35.0    28.0  37.0        40.0
    Programmer   31.0    29.0   NaN         NaN

Multiple columns can be specified in any of the attributes index, columns and values.

    print (df.pivot_table(index=['Position','Sex'], columns='City', values='Age', aggfunc='first'))

    City               Boston  Chicago  Los Angeles
    Position   Sex
    Manager    Female    35.0     28.0         40.0
               Male       NaN     37.0          NaN
    Programmer Female    31.0     29.0          NaN

**Applying several aggregating functions**

You can easily apply multiple functions during a single pivot:

    In [23]: import numpy as np
    
    In [24]: df.pivot_table(index='Position', values='Age', aggfunc=[np.mean, np.std])
    Out[24]: 
                     mean       std
    Position                       
    Manager     34.333333  5.507571
    Programmer  32.333333  4.163332

Sometimes, you may want to apply specific functions to specific columns:

    In [35]: df['Random'] = np.random.random(6)
    In [36]: df
    Out[36]: 
       Name    Position         City  Age    Random
    0  Mary     Manager       Boston   34  0.678577
    1  Josh  Programmer     New York   37  0.973168
    2   Jon     Manager      Chicago   29  0.146668
    3  Lucy     Manager  Los Angeles   40  0.150120
    4  Jane  Programmer      Chicago   29  0.112769
    5   Sue  Programmer       Boston   31  0.185198
    
    For example, find the mean age, and standard deviation of random by Position:

    In [37]: df.pivot_table(index='Position', aggfunc={'Age': np.mean, 'Random': np.std})
    Out[37]: 
                      Age    Random
    Position                       
    Manager     34.333333  0.306106
    Programmer  32.333333  0.477219

One can pass a list of functions to apply to the individual columns as well:

    In [38]: df.pivot_table(index='Position', aggfunc={'Age': np.mean, 'Random': [np.mean, np.std]})]
    Out[38]: 
                      Age    Random          
                     mean      mean       std
    Position                                 
    Manager     34.333333  0.325122  0.306106
    Programmer  32.333333  0.423712  0.477219



## Stacking and unstacking
    import pandas as pd
    import numpy as np
    
    np.random.seed(0)
    tuples = list(zip(*[['bar', 'bar', 'foo', 'foo', 'qux', 'qux'],
                        ['one', 'two', 'one', 'two','one', 'two']]))
    
    idx = pd.MultiIndex.from_tuples(tuples, names=['first', 'second'])
    df = pd.DataFrame(np.random.randn(6, 2), index=idx, columns=['A', 'B'])
    print (df)
                         A         B
    first second                    
    bar   one     1.764052  0.400157
          two     0.978738  2.240893
    foo   one     1.867558 -0.977278
          two     0.950088 -0.151357
    qux   one    -0.103219  0.410599
          two     0.144044  1.454274

<!-- -->
    
    print (df.stack())
    first  second   
    bar    one     A    1.764052
                   B    0.400157
           two     A    0.978738
                   B    2.240893
    foo    one     A    1.867558
                   B   -0.977278
           two     A    0.950088
                   B   -0.151357
    qux    one     A   -0.103219
                   B    0.410599
           two     A    0.144044
                   B    1.454274
    dtype: float64
    
    #reset index, rename column name
    print (df.stack().reset_index(name='val2').rename(columns={'level_2': 'val1'}))
       first second val1      val2
    0    bar    one    A  1.764052
    1    bar    one    B  0.400157
    2    bar    two    A  0.978738
    3    bar    two    B  2.240893
    4    foo    one    A  1.867558
    5    foo    one    B -0.977278
    6    foo    two    A  0.950088
    7    foo    two    B -0.151357
    8    qux    one    A -0.103219
    9    qux    one    B  0.410599
    10   qux    two    A  0.144044
    11   qux    two    B  1.454274

---
    
    print (df.unstack())
                   A                   B          
    second       one       two       one       two
    first                                         
    bar     1.764052  0.978738  0.400157  2.240893
    foo     1.867558  0.950088 -0.977278 -0.151357
    qux    -0.103219  0.144044  0.410599  1.454274


[`rename_axis`](http://pandas.pydata.org/pandas-docs/stable/whatsnew.html#changes-to-rename) (new in `pandas` `0.18.0`):
    
    #reset index, remove columns names 
    df1 = df.unstack().reset_index().rename_axis((None,None), axis=1)
    #reset MultiIndex in columns with list comprehension
    df1.columns = ['_'.join(col).strip('_') for col in df1.columns]
    print (df1)
      first     A_one     A_two     B_one     B_two
    0   bar  1.764052  0.978738  0.400157  2.240893
    1   foo  1.867558  0.950088 -0.977278 -0.151357
    2   qux -0.103219  0.144044  0.410599  1.454274

*pandas bellow 0.18.0*

    #reset index
    df1 = df.unstack().reset_index()
    #remove columns names
    df1.columns.names = (None, None)
    #reset MultiIndex in columns with list comprehension
    df1.columns = ['_'.join(col).strip('_') for col in df1.columns]
    print (df1)
      first     A_one     A_two     B_one     B_two
    0   bar  1.764052  0.978738  0.400157  2.240893
    1   foo  1.867558  0.950088 -0.977278 -0.151357
    2   qux -0.103219  0.144044  0.410599  1.454274


## Simple pivoting
First try use [`pivot`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.pivot.html):

    import pandas as pd
    import numpy as np
    
    df = pd.DataFrame({'Name':['Mary', 'Josh','Jon','Lucy', 'Jane', 'Sue'],
                       'Age':[34, 37, 29, 40, 29, 31],
                       'City':['Boston','New York', 'Chicago', 'Los Angeles', 'Chicago', 'Boston'],
                       'Position':['Manager','Programmer','Manager','Manager','Programmer', 'Programmer']},
                        columns=['Name','Position','City','Age'])
    
    print (df)
       Name    Position         City  Age
    0  Mary     Manager       Boston   34
    1  Josh  Programmer     New York   37
    2   Jon     Manager      Chicago   29
    3  Lucy     Manager  Los Angeles   40
    4  Jane  Programmer      Chicago   29
    5   Sue  Programmer       Boston   31

    print (df.pivot(index='Position', columns='City', values='Age'))
    City        Boston  Chicago  Los Angeles  New York
    Position                                          
    Manager       34.0     29.0         40.0       NaN
    Programmer    31.0     29.0          NaN      37.0

If need reset index, remove columns names and fill NaN values:
    
    #pivoting by numbers - column Age
    print (df.pivot(index='Position', columns='City', values='Age')
             .reset_index()
             .rename_axis(None, axis=1)
             .fillna(0))
             
         Position  Boston  Chicago  Los Angeles  New York
    0     Manager    34.0     29.0         40.0       0.0
    1  Programmer    31.0     29.0          0.0      37.0


    #pivoting by strings - column Name
    print (df.pivot(index='Position', columns='City', values='Name'))   
          
    City       Boston Chicago Los Angeles New York
    Position                                      
    Manager      Mary     Jon        Lucy     None
    Programmer    Sue    Jane        None     Josh

## Cross Tabulation
    import pandas as pd
    df = pd.DataFrame({'Sex': ['M', 'M', 'F', 'M', 'F', 'F', 'M', 'M', 'F', 'F'], 
                   'Age': [20, 19, 17, 35, 22, 22, 12, 15, 17, 22],
                   'Heart Disease': ['Y', 'N', 'Y', 'N', 'N', 'Y', 'N', 'Y', 'N', 'Y']})

    df
  
      Age Heart Disease Sex
    0   20             Y   M
    1   19             N   M
    2   17             Y   F
    3   35             N   M
    4   22             N   F
    5   22             Y   F
    6   12             N   M
    7   15             Y   M
    8   17             N   F
    9   22             Y   F

    pd.crosstab(df['Sex'], df['Heart Disease'])
    
    Hearth Disease  N  Y
    Sex                 
    F               2  3
    M               3  2

Using dot notation:

    pd.crosstab(df.Sex, df.Age)
    
    Age  12  15  17  19  20  22  35
    Sex                            
    F     0   0   2   0   0   3   0
    M     1   1   0   1   1   0   1
    
 Getting transpose of DF:

    pd.crosstab(df.Sex, df.Age).T
    
    Sex  F  M
    Age      
    12   0  1
    15   0  1
    17   2  0
    19   0  1
    20   0  1
    22   3  0
    35   0  1
    
Getting margins or cumulatives:

    pd.crosstab(df['Sex'], df['Heart Disease'], margins=True)
    
    Heart Disease  N  Y  All
    Sex                     
    F              2  3    5
    M              3  2    5
    All            5  5   10
    
 Getting transpose of cumulative:

    pd.crosstab(df['Sex'], df['Age'], margins=True).T
    
    
    Sex  F  M  All
    Age           
    12   0  1    1
    15   0  1    1
    17   2  0    2
    19   0  1    1
    20   0  1    1
    22   3  0    3
    35   0  1    1
    All  5  5   10
    
Getting percentages :    

    pd.crosstab(df["Sex"],df['Heart Disease']).apply(lambda r: r/len(df), axis=1)
    
    Heart Disease    N    Y
    Sex                    
    F              0.2  0.3
    M              0.3  0.2
    
Getting cumulative and multiplying by 100:
    
    df2 = pd.crosstab(df["Age"],df['Sex'], margins=True ).apply(lambda r: r/len(df)*100, axis=1)
    
    df2
    
    Sex     F     M    All
    Age                   
    12    0.0  10.0   10.0
    15    0.0  10.0   10.0
    17   20.0   0.0   20.0
    19    0.0  10.0   10.0
    20    0.0  10.0   10.0
    22   30.0   0.0   30.0
    35    0.0  10.0   10.0
    All  50.0  50.0  100.0

Removing a column from DF (one way):
    
    df2[["F","M"]]
    
    Sex     F     M
    Age            
    12    0.0  10.0
    15    0.0  10.0
    17   20.0   0.0
    19    0.0  10.0
    20    0.0  10.0
    22   30.0   0.0
    35    0.0  10.0
    All  50.0  50.0














## Pandas melt to go from wide to long
    >>> df
       ID  Year  Jan_salary  Feb_salary  Mar_salary
    0   1  2016        4500        4200        4700
    1   2  2016        3800        3600        4400
    2   3  2016        5500        5200        5300
    
    >>> melted_df = pd.melt(df,id_vars=['ID','Year'],
                            value_vars=['Jan_salary','Feb_salary','Mar_salary'],
                            var_name='month',value_name='salary')
    
    >>> melted_df
       ID  Year       month  salary
    0   1  2016  Jan_salary    4500
    1   2  2016  Jan_salary    3800
    2   3  2016  Jan_salary    5500
    3   1  2016  Feb_salary    4200
    4   2  2016  Feb_salary    3600
    5   3  2016  Feb_salary    5200
    6   1  2016  Mar_salary    4700
    7   2  2016  Mar_salary    4400
    8   3  2016  Mar_salary    5300
    
    >>> melted_['month'] = melted_['month'].str.replace('_salary','')
    
    >>> import calendar
    >>> def mapper(month_abbr):
    ...     # from http://stackoverflow.com/a/3418092/42346
    ...     d = {v: str(k).zfill(2) for k,v in enumerate(calendar.month_abbr)}
    ...     return d[month_abbr]
    
    >>> melted_df['month'] = melted_df['month'].apply(mapper)
    >>> melted_df
       ID  Year month  salary
    0   1  2016    01    4500
    1   2  2016    01    3800
    2   3  2016    01    5500
    3   1  2016    02    4200
    4   2  2016    02    3600
    5   3  2016    02    5200
    6   1  2016    03    4700
    7   2  2016    03    4400
    8   3  2016    03    5300

## Split (reshape) CSV strings in columns into multiple rows, having one element per row
    import pandas as pd
    
    df = pd.DataFrame([{'var1': 'a,b,c', 'var2': 1, 'var3': 'XX'},
                       {'var1': 'd,e,f,x,y', 'var2': 2, 'var3': 'ZZ'}])

    print(df)

    reshaped = \
    (df.set_index(df.columns.drop('var1',1).tolist())
       .var1.str.split(',', expand=True)
       .stack()
       .reset_index()
       .rename(columns={0:'var1'})
       .loc[:, df.columns]
    )
    
    print(reshaped)

Output:

            var1  var2 var3
    0      a,b,c     1   XX
    1  d,e,f,x,y     2   ZZ
    
      var1  var2 var3
    0    a     1   XX
    1    b     1   XX
    2    c     1   XX
    3    d     2   ZZ
    4    e     2   ZZ
    5    f     2   ZZ
    6    x     2   ZZ
    7    y     2   ZZ



