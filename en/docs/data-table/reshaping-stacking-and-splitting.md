---
title: "Reshaping, stacking and splitting"
slug: "reshaping-stacking-and-splitting"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

The official vignette, ["Efficient reshaping using data.tables"][1], is the best introduction to this topic.

Many reshaping tasks require moving between long and wide formats:
 - Wide data is data with each column representing a seperate variable, and rows representing seperate observations
 - Long data is data with the form ID | variable | value, where each row representing a observation-variable pair


  [1]: https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-reshape.html

## melt and cast with data.table


## Going from long to wide format using dcast
# Casting: The Basics

Casting is used to transform data from long to wide format.

Starting with a long data set: 

    DT = data.table(ID = rep(letters[1:3],3), Age = rep(20:22,3), Test = rep(c("OB_A","OB_B","OB_C"), each = 3), Result = 1:9)

We can cast our data using the `dcast` function in data.table. This returns another data.table in wide format:

    dcast(DT, formula = ID ~ Test, value.var = "Result")
       ID OB_A OB_B OB_C
    1:  a    1    4    7
    2:  b    2    5    8
    3:  c    3    6    9

    class(dcast(DT, formula = ID ~ Test, value.var = "Result"))
    [1] "data.table" "data.frame"

# Casting a value

A `value.var` argument is necessary for a proper cast - if not provided dcast will make an assumption based on your data.

    dcast(DT, formula = ID ~ Test, value.var = "Result")
       ID OB_A OB_B OB_C
    1:  a    1    4    7
    2:  b    2    5    8
    3:  c    3    6    9
    
       ID OB_A OB_B OB_C
    1:  a   20   20   20
    2:  b   21   21   21
    3:  c   22   22   22

Multiple `value.var`s can be provided in a list

    dcast(DT, formula = ID ~ Test, value.var = list("Result","Age"))
       ID Result_OB_A Result_OB_B Result_OB_C Age_OB_A Age_OB_B Age_OB_C
    1:  a           1           4           7       20       20       20
    2:  b           2           5           8       21       21       21
    3:  c           3           6           9       22       22       22

# Formula

Casting is controlled using the formula argument in `dcast`. This is of the form ROWS ~ COLUMNS

    dcast(DT, formula = ID ~ Test, value.var = "Result")
       ID OB_A OB_B OB_C
    1:  a    1    4    7
    2:  b    2    5    8
    3:  c    3    6    9

    dcast(DT, formula = Test ~ ID, value.var = "Result")
       Test a b c
    1: OB_A 1 2 3
    2: OB_B 4 5 6
    3: OB_C 7 8 9

Both rows and columns can be expanded with further variables using `+`

    dcast(DT, formula = ID + Age ~ Test, value.var = "Result")
       ID Age OB_A OB_B OB_C
    1:  a  20    1    4    7
    2:  b  21    2    5    8
    3:  c  22    3    6    9

    dcast(DT, formula = ID ~ Age + Test, value.var = "Result")
       ID 20_OB_A 20_OB_B 20_OB_C 21_OB_A 21_OB_B 21_OB_C 22_OB_A 22_OB_B 22_OB_C
    1:  a       1       4       7      NA      NA      NA      NA      NA      NA
    2:  b      NA      NA      NA       2       5       8      NA      NA      NA
    3:  c      NA      NA      NA      NA      NA      NA       3       6       9

    #order is important

    dcast(DT, formula = ID ~ Test + Age, value.var = "Result")
       ID OB_A_20 OB_A_21 OB_A_22 OB_B_20 OB_B_21 OB_B_22 OB_C_20 OB_C_21 OB_C_22
    1:  a       1      NA      NA       4      NA      NA       7      NA      NA
    2:  b      NA       2      NA      NA       5      NA      NA       8      NA
    3:  c      NA      NA       3      NA      NA       6      NA      NA       9

Casting can often create cells where no observation exists in the data. By default this is denoted by `NA`, as above. We can override this with the `fill=` argument.

    dcast(DT, formula = ID ~ Test + Age, value.var = "Result", fill = 0)
       ID OB_A_20 OB_A_21 OB_A_22 OB_B_20 OB_B_21 OB_B_22 OB_C_20 OB_C_21 OB_C_22
    1:  a       1       0       0       4       0       0       7       0       0
    2:  b       0       2       0       0       5       0       0       8       0
    3:  c       0       0       3       0       0       6       0       0       9

You can also use two special variables in the formula object

- `.` represents no other variables
- `...` represents all other variables


    dcast(DT, formula = Age ~ ., value.var = "Result")
       Age .
    1:  20 3
    2:  21 3
    3:  22 3

    dcast(DT, formula = ID + Age ~ ..., value.var = "Result")
       ID Age OB_A OB_B OB_C
    1:  a  20    1    4    7
    2:  b  21    2    5    8
    3:  c  22    3    6    9

# Aggregating our value.var

We can also cast and aggregate values in one step. In this case, we have three observations in each of the intersections of Age and ID. To set what aggregation we want, we use the `fun.aggregate` argument:

    #length
    dcast(DT, formula = ID ~ Age, value.var = "Result", fun.aggregate = length)
       ID 20 21 22
    1:  a  3  0  0
    2:  b  0  3  0
    3:  c  0  0  3
    
    #sum
    dcast(DT, formula = ID ~ Age, value.var = "Result", fun.aggregate = sum)
       ID 20 21 22
    1:  a 12  0  0
    2:  b  0 15  0
    3:  c  0  0 18
    
    #concatenate
    dcast(DT, formula = ID ~ Age, value.var = "Result", fun.aggregate = function(x){paste(x,collapse = "_")})
    ID    20    21    22
    1:  a 1_4_7            
    2:  b       2_5_8      
    3:  c             3_6_9

We can also pass a list to `fun.aggregate` to use multiple functions

    dcast(DT, formula = ID ~ Age, value.var = "Result", fun.aggregate = list(sum,length))
       ID Result_sum_20 Result_sum_21 Result_sum_22 Result_length_20 Result_length_21 Result_length_22
    1:  a            12             0             0                3                0                0
    2:  b             0            15             0                0                3                0
    3:  c             0             0            18                0                0                3

If we pass more than one function and more than one value, we can calculate all combinations by passing a vector of value.vars

    dcast(DT, formula = ID ~ Age, value.var = c("Result","Test"), fun.aggregate = list(function(x){paste0(x,collapse = "_")},length))
       ID Result_function_20 Result_function_21 Result_function_22 Test_function_20 Test_function_21 Test_function_22 Result_length_20 Result_length_21
    1:  a              1_4_7                                         OB_A_OB_B_OB_C                                                  3                0
    2:  b                                 2_5_8                                       OB_A_OB_B_OB_C                                 0                3
    3:  c                                                    3_6_9                                     OB_A_OB_B_OB_C                0                0
       Result_length_22 Test_length_20 Test_length_21 Test_length_22
    1:                0              3              0              0
    2:                0              0              3              0
    3:                3              0              0              3

where each pair is calculated in the order `value1_formula1, value1_formula2, ... , valueN_formula(N-1), valueN_formulaN`.

Alternatively, we can evaluate our values and functions one-to-one by passing 'value.var' as a list:

    dcast(DT, formula = ID ~ Age, value.var = list("Result","Test"), fun.aggregate = list(function(x){paste0(x,collapse = "_")},length))
       ID Result_function_20 Result_function_21 Result_function_22 Test_length_20 Test_length_21 Test_length_22
    1:  a              1_4_7                                                    3              0              0
    2:  b                                 2_5_8                                 0              3              0
    3:  c                                                    3_6_9              0              0              3


# Naming columns in the result

By default, column name components are seperated by an underscore `_`. This can be manually overridden using the `sep=` argument:

    dcast(DT, formula = Test ~ ID + Age, value.var = "Result")
    Test a_20 b_21 c_22
    1: OB_A    1    2    3
    2: OB_B    4    5    6
    3: OB_C    7    8    9
    
    dcast(DT, formula = Test ~ ID + Age, value.var = "Result", sep = ",")
       Test a,20 b,21 c,22
    1: OB_A    1    2    3
    2: OB_B    4    5    6
    3: OB_C    7    8    9

This will seperate any `fun.aggregate` or `value.var` we use:

    dcast(DT, formula = Test ~ ID + Age, value.var = "Result", fun.aggregate = c(sum,length), sep = ",")
       Test Result,sum,a,20 Result,sum,b,21 Result,sum,c,22 Result,length,a,20 Result,length,b,21 Result,length,c,22
    1: OB_A               1               2               3                  1                  1                  1
    2: OB_B               4               5               6                  1                  1                  1
    3: OB_C               7               8               9                  1                  1                  1


## Going from wide to long format using melt
# Melting: The basics
Melting is used to transform data from wide to long format. 

Starting with a wide data set:

    DT = data.table(ID = letters[1:3], Age = 20:22, OB_A = 1:3, OB_B = 4:6, OB_C = 7:9)

We can melt our data using the `melt` function in data.table. This returns another data.table in long format:

    melt(DT, id.vars = c("ID","Age"))
    1:  a  20     OB_A     1
    2:  b  21     OB_A     2
    3:  c  22     OB_A     3
    4:  a  20     OB_B     4
    5:  b  21     OB_B     5
    6:  c  22     OB_B     6
    7:  a  20     OB_C     7
    8:  b  21     OB_C     8
    9:  c  22     OB_C     9

    class(melt(DT, id.vars = c("ID","Age")))
    # "data.table" "data.frame"

Any columns not set in the `id.vars` parameter are assumed to be variables. Alternatively, we can set these explicitly using the `measure.vars` argument:

    melt(DT, measure.vars = c("OB_A","OB_B","OB_C"))
       ID Age variable value
    1:  a  20     OB_A     1
    2:  b  21     OB_A     2
    3:  c  22     OB_A     3
    4:  a  20     OB_B     4
    5:  b  21     OB_B     5
    6:  c  22     OB_B     6
    7:  a  20     OB_C     7
    8:  b  21     OB_C     8
    9:  c  22     OB_C     9

In this case, any columns not set in `measure.vars` are assumed to be IDs.

If we set both explicitly, it will only return the columns selected:

    melt(DT, id.vars = "ID", measure.vars = c("OB_C"))
       ID variable value
    1:  a     OB_C     7
    2:  b     OB_C     8
    3:  c     OB_C     9

# Naming variables and values in the result

We can manipulate the column names of the returned table using `variable.name` and `value.name`

    melt(DT,
         id.vars = c("ID"), 
         measure.vars = c("OB_C"), 
         variable.name = "Test", 
         value.name = "Result"
         )
       ID Test Result
    1:  a OB_C      7
    2:  b OB_C      8
    3:  c OB_C      9
    
# Setting types for measure variables in the result

By default, melting a data.table converts all `measure.vars` to factors:

    M_DT <- melt(DT,id.vars = c("ID"), measure.vars = c("OB_C"))
    class(M_DT[, variable])
    # "factor"

To set as character instead, use the `variable.factor` argument:

    M_DT <- melt(DT,id.vars = c("ID"), measure.vars = c("OB_C"), variable.factor = FALSE)
    class(M_DT[, variable])
    # "character"

Values generally inherit from the data type of the originating column:

    class(DT[, value])
    # "integer"
    class(M_DT[, value])
    # "integer"

If there is a conflict, data types will be coerced. For example:

    M_DT <- melt(DT,id.vars = c("Age"), measure.vars = c("ID","OB_C"))
    class(M_DT[, value])
    # "character"

When melting, any factor variables will be coerced to character type:

    DT[, OB_C := factor(OB_C)]
    M_DT <- melt(DT,id.vars = c("ID"), measure.vars = c("OB_C"))
    class(M_DT)
    # "character"

To avoid this and preserve the initial typing, use the `value.factor` argument:

    M_DT <- melt(DT,id.vars = c("ID"), measure.vars = c("OB_C"), value.factor = TRUE)
    class(M_DT)
    # "factor"

# Handling missing values

By default, any `NA` values are preserved in the molten data

    DT = data.table(ID = letters[1:3], Age = 20:22, OB_A = 1:3, OB_B = 4:6, OB_C = c(7:8,NA))
    melt(DT,id.vars = c("ID"), measure.vars = c("OB_C"))
       ID variable value
    1:  a     OB_C     7
    2:  b     OB_C     8
    3:  c     OB_C    NA

If these should be removed from your data, set `na.rm = TRUE`

    melt(DT,id.vars = c("ID"), measure.vars = c("OB_C"), na.rm = TRUE)
       ID variable value
    1:  a     OB_C     7
    2:  b     OB_C     8
    

## Stacking multiple tables using rbindlist
A common refrain in R goes along these lines:

> You should not have a bunch of related tables with names like `DT1`, `DT2`, ..., `DT11`. Iteratively reading and assigning to objects by name is messy. The solution is a list of tables of data!

Such a list looks like

    set.seed(1)
    DT_list = lapply(setNames(1:3, paste0("D", 1:3)), function(i)
      data.table(id = 1:2, v = sample(letters, 2)))
    
    $D1
       id v
    1:  1 g
    2:  2 j
    
    $D2
       id v
    1:  1 o
    2:  2 w
    
    $D3
       id v
    1:  1 f
    2:  2 w
    
Another perspective is that you should store these tables together *as one table*, by stacking them. This is straightforward to do using `rbindlist`:

    DT = rbindlist(DT_list, id="src")

       src id v
    1:  D1  1 g
    2:  D1  2 j
    3:  D2  1 o
    4:  D2  2 w
    5:  D3  1 f
    6:  D3  2 w

This format makes a lot more sense with data.table syntax, where "by group" operations are common and straightforward. 

For a deeper look, [Gregor's answer][1] might be a good place to start. Also check out `?rbindlist`, of course. There's a separate example covering [reading in a bunch of tables from CSV and then stacking them][2].


  [1]: http://stackoverflow.com/a/24376207/
  [2]: https://www.wikiod.com/data-table/using-list-columns-to-store-data#Reading in many related files

## Reshape using `data.table`

`data.table` extends `reshape2`'s `melt` & `dcast` functions


([Reference: Efficient reshaping using data.tables](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html))

    library(data.table)
    
    ## generate some data
    dt <- data.table(
      name = rep(c("firstName", "secondName"), each=4),
      numbers = rep(1:4, 2),
      value = rnorm(8)
    )
    dt
    #          name numbers      value
    # 1:  firstName       1 -0.8551881
    # 2:  firstName       2 -1.0561946
    # 3:  firstName       3  0.2671833
    # 4:  firstName       4  1.0662379
    # 5: secondName       1 -0.4771341
    # 6: secondName       2  1.2830651
    # 7: secondName       3 -0.6989682
    # 8: secondName       4 -0.6592184
    
 **Long to Wide**
    
    dcast(data = dt, 
          formula = name ~ numbers, 
          value.var = "value")
    
    #          name          1          2         3         4
    # 1:  firstName  0.1836433 -0.8356286 1.5952808 0.3295078
    # 2: secondName -0.8204684  0.4874291 0.7383247 0.5757814
    
 On multiple columns (as of `data.table` 1.9.6)
    
    ## add an extra column
    dt[, value2 := value * 2]
    
    ## cast multiple value columns
    dcast(data = dt, 
          formula = name ~ numbers, 
          value.var = c("value", "value2"))
    
    #          name    value_1    value_2   value_3   value_4   value2_1   value2_2 value2_3  value2_4
    # 1:  firstName  0.1836433 -0.8356286 1.5952808 0.3295078  0.3672866 -1.6712572 3.190562 0.6590155
    # 2: secondName -0.8204684  0.4874291 0.7383247 0.5757814 -1.6409368  0.9748581 1.476649 1.1515627
    
    
**Wide to Long**
      
    ## use a wide data.table
    dt <- fread("name          1          2         3         4
    firstName  0.1836433 -0.8356286 1.5952808 0.3295078
    secondName -0.8204684  0.4874291 0.7383247 0.5757814", header = T)
    dt
    #          name          1          2         3         4
    # 1:  firstName  0.1836433 -0.8356286 1.5952808 0.3295078
    # 2: secondName -0.8204684  0.4874291 0.7383247 0.5757814
    
    ## melt to long, specifying the id column, and the name of the columns 
    ## in the resulting long data.table
    melt(dt, 
         id.vars = "name", 
         variable.name = "numbers",
         value.name = "myValue")
    #          name  numbers    myValue
    # 1:  firstName        1  0.1836433
    # 2: secondName        1 -0.8204684
    # 3:  firstName        2 -0.8356286
    # 4: secondName        2  0.4874291
    # 5:  firstName        3  1.5952808
    # 6: secondName        3  0.7383247
    # 7:  firstName        4  0.3295078
    # 8: secondName        4  0.5757814

