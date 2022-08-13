---
title: "Lists"
slug: "lists"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Quick Introduction to Lists
In general, most of the objects you would interact with as a user would tend to be a vector; e.g numeric vector, logical vector. These objects can only take in a single type of variable (a numeric vector can only have numbers inside it).

A list would be able to store any type variable in it, making it to the generic object that can store any type of variables we would need.

Example of initializing a list
    
    exampleList1 <- list('a', 'b')
    exampleList2 <- list(1, 2)
    exampleList3 <- list('a', 1, 2)

In order to understand the data that was defined in the list, we can use the str function.

    str(exampleList1)
    str(exampleList2)
    str(exampleList3)

Subsetting of lists distinguishes between extracting a slice of the list, i.e. obtaining a list containing a subset of the elements in the original list, and extracting a single element. Using the `[` operator commonly used for vectors produces a new list.

    # Returns List
    exampleList3[1]
    exampleList3[1:2]

To obtain a single element use `[[` instead.

    # Returns Character
    exampleList3[[1]]

List entries may be named:

    exampleList4 <- list(
        num = 1:3,
        numeric = 0.5,
        char = c('a', 'b')
    )

The entries in named lists can be accessed by their name instead of their index.

    exampleList4[['char']]

Alternatively the `$` operator can be used to access named elements.

    exampleList4$num

This has the advantage that it is faster to type and may be easier to read but it is important to be aware of a potential pitfall. The `$` operator uses partial matching to identify matching list elements and may produce unexpected results.

    exampleList5 <- exampleList4[2:3]
    
    exampleList4$num
    # c(1, 2, 3)
    
    exampleList5$num
    # 0.5

    exampleList5[['num']]
    # NULL
    
Lists can be particularly useful because they can store objects of different lengths and of various classes.

    ## Numeric vector
    exampleVector1 <- c(12, 13, 14)
    ## Character vector
    exampleVector2 <- c("a", "b", "c", "d", "e", "f")
    ## Matrix
    exampleMatrix1 <- matrix(rnorm(4), ncol = 2, nrow = 2)
    ## List
    exampleList3 <- list('a', 1, 2)

    exampleList6 <- list(
        num = exampleVector1, 
        char = exampleVector2,
        mat = exampleMatrix1, 
        list = exampleList3
    )
    exampleList6
    #$num
    #[1] 12 13 14
    #
    #$char
    #[1] "a" "b" "c" "d" "e" "f"
    #
    #$mat
    #          [,1]        [,2]
    #[1,] 0.5013050 -1.88801542
    #[2,] 0.4295266  0.09751379
    #
    #$list
    #$list[[1]]
    #[1] "a"
    #
    #$list[[2]]
    #[1] 1
    #
    #$list[[3]]
    #[1] 2

## Introduction to lists


## Serialization: using lists to pass informations
There exist cases in which it is necessary to put data of different types together. In Azure ML for example, it is necessary to pass informations from a R script module to another one exclusively throught dataframes.
Suppose we have a dataframe and a number: 

    > df
           name height        team fun_index title age         desc Y
    1    Andrea    195       Lazio        97     6  33   eccellente 1
    2      Paja    165  Fiorentina        87     6  31       deciso 1
    3      Roro    190       Lazio        65     6  28       strano 0
    4    Gioele     70       Lazio       100     0   2    simpatico 1
    5     Cacio    170    Juventus        81     3  33         duro 0
    6     Edola    171       Lazio        72     5  32     svampito 1
    7    Salami    175       Inter        75     3  30  doppiopasso 1
    8    Braugo    180       Inter        79     5  32          gjn 0
    9     Benna    158    Juventus        80     6  28     esaurito 0
    10   Riggio    182       Lazio        92     5  31     certezza 1
    11 Giordano    185        Roma        79     5  29        buono 1

    > number <- "42"

We can access to this information:

    > paste(df$name[4],"is a",df3$team[4], "supporter." )
    [1] "Gioele is a  Lazio supporter."
    > paste("The answer to THE question is", number )
    [1] "The answer to THE question is 42"

In order to put different types of data in a dataframe we have to use the list object and the serialization. In particular we have to put the data in a generic list and then put the list in a particular dataframe:

    l <- list(df,number)
    dataframe_container <- data.frame(out2 = as.integer(serialize(l, connection=NULL)))

Once we have stored the information in the dataframe, we need to deserialize it in order to use it:

    #----- unserialize ----------------------------------------+
    unser_obj <- unserialize(as.raw(dataframe_container$out2))
    #----- taking back the elements----------------------------+
    df_mod        <- unser_obj[1][[1]]  
    number_mod    <- unser_obj[2][[1]]
Then, we can verify that the data are transfered correctly:

    > paste(df_mod$name[4],"is a",df_mod$team[4], "supporter." )
    [1] "Gioele is a  Lazio supporter."
    > paste("The answer to THE question is", number_mod )
    [1] "The answer to THE question is 42"



## Reasons for using lists
To the average R user, the list structure may appear to be the one of the more complicated data structures to manipulate. There are no guarantees that all the elements within it are of the same type; There is no guaranteed structure of how complicated/non-complicated that the list would be (An element in a list could be a list)

However, one of the main reasons when to use lists to use it to pass parameters between functions.

    # Function example which returns a single element numeric vector
    exampleFunction1 <- function(num1, num2){
        result <- num1 + num2
        return(result)
    }

    # Using example function 1
    exampleFunction1(1, 2)

    # Function example which returns a simple numeric vector
    exampleFunction2 <- function(num1, num2, multiplier){
        tempResult1 <- num1 + num2
        tempResult2 <- tempResult1 * multiplier
        result <- c(tempResult1, tempResult2)
        return(result) 
    }
    
    # Using example function 2
    exampleFunction2(1, 2, 4)

In the above example, the returned results are just simple numeric vectors. There is no issues to pass over such simple vectors.

It is important to note at this point that generally, R functions only return 1 result at a time (You can use if conditions to return different results). However, if you intend to create a function which takes a set of parameters and returns several type of results such a numeric vector(settings value) and a data frame (from the calculation), you would need to dump all these results in a list before returning it.

    # We will be using mtcars dataset here
    # Function which returns a result that is supposed to contain multiple type of results
    # This can be solved by putting the results into a list
    exampleFunction3 <- function(dataframe, removeColumn, sumColumn){
        resultDataFrame <- dataframe[, -removeColumn]
        resultSum <- sum(dataframe[, sumColumn])
        resultList <- list(resultDataFrame, resultSum)
        return(resultList)
    }

    # Using example function 3
    exampleResult <- exampleFunction3(mtcars, 2, 4)
    exampleResult[[1]]
    exampleResult[[2]]

## Convert a list to a vector while keeping empty list elements
When one wishes to convert a list to a vector or data.frame object empty elements are typically dropped.

This can be problematic which a list is created of a desired length are created with some empty values (e.g. a list with n elements is created to be added to an m x n matrix, data.frame, or data.table). It is possible to losslessly convert a list to a vector however, retaining empty elements: 

    res <- list(character(0), c("Luzhuang", "Laisu", "Peihui"), character(0), 
        c("Anjiangping", "Xinzhai", "Yongfeng"), character(0), character(0), 
        c("Puji", "Gaotun", "Banjingcun"), character(0), character(0), 
        character(0))
    res

>     [[1]]
>     character(0)
>     
>     [[2]]
>     [1] "Luzhuang" "Laisu"    "Peihui"  
>     
>     [[3]]
>     character(0)
>     
>     [[4]]
>     [1] "Anjiangping" "Xinzhai"     "Yongfeng"   
>     
>     [[5]]
>     character(0)
>     
>     [[6]]
>     character(0)
>     
>     [[7]]
>     [1] "Puji"       "Gaotun"     "Banjingcun"
>     
>     [[8]]
>     character(0)
>     
>     [[9]]
>     character(0)
>     
>     [[10]]
>     character(0)

    res <- sapply(res, function(s) if (length(s) == 0) NA_character_ else paste(s, collapse = " "))
    res

>      [1] NA                             "Luzhuang Laisu Peihui"        NA                             "Anjiangping Xinzhai Yongfeng" NA      
> 
>      [6] NA                             "Puji Gaotun Banjingcun"       NA                             NA                             NA

