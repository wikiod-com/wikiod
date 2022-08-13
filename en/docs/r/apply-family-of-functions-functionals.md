---
title: "*apply family of functions (functionals)"
slug: "apply-family-of-functions-functionals"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

A function in the `*apply` family is an abstraction of a `for` loop. Compared with the `for` loops `*apply` functions have the following advantages:

 1. Require less code to write.
 2. Doesn't have an iteration counter.
 3. Doesn't use temporary variables to store intermediate results.

However `for` loops are more general and can give us more control allowing to achieve complex computations that are not always trivial to do using `*apply` functions. 

The relationship between `for` loops and `*apply` functions is explained in the [documentation for `for` loops][1].

# Members of the `*apply` Family

The `*apply` family of functions contains several variants of the same principle that differ based primarily on the kind of output they return.

| function | Input | Output |
| -------- | ------| ------ |
| `apply`  | `matrix`, `data.frame`, or `array` | vector or matrix (depending on the length of each element returned) |
| `sapply` | vector or `list` | vector or matrix (depending on the length of each element returned) |
| `lapply` | vector or `list` | `list` |
| `vapply` | vector or `list | vector or matrix (depending on the length of each element returned) of the user-designated class |
| `mapply` | multiple vectors, `lists` or a combination | `list` |

See "Examples" to see how each of these functions is used.

  [1]: https://www.wikiod.com/r/control-flow-structures

## Using built-in functionals
# Built-in functionals: lapply(), sapply(), and mapply()

R comes with built-in functionals, of which perhaps the most well-known are the apply family of functions. Here is a description of some of the most common apply functions:

* `lapply()`    = takes a list as an argument and applies the specified function to the list.
* `sapply()` = the same as `lapply()` but attempts to simplify the output to a vector or a matrix.
    * `vapply()` = a variant of `sapply()` in which the output object's type must be specified.
* `mapply()` = like `lapply()` but can pass multiple vectors as input to the specified function. Can be simplified like `sapply()`.
    * `Map()` is an alias to `mapply()` with `SIMPLIFY = FALSE`.
___
### lapply()
`lapply()` can be used with two different iterations:
* `lapply(variable, FUN)`
* `lapply(seq_along(variable), FUN)`



    # Two ways of finding the mean of x
    set.seed(1)
    df <- data.frame(x = rnorm(25), y = rnorm(25))
    lapply(df, mean)
    lapply(seq_along(df), function(x) mean(df[[x]))
___
### sapply()
`sapply()` will attempt to resolve its output to either a vector or a matrix.

    # Two examples to show the different outputs of sapply()
    sapply(letters, print)  ## produces a vector
    x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
    sapply(x, quantile)  ## produces a matrix
___
### mapply()
`mapply()` works much like `lapply()` except it can take multiple vectors as input (hence the m for multivariate).

    mapply(sum, 1:5, 10:6, 3) # 3 will be "recycled" by mapply
___

  [1]: http://adv-r.had.co.nz/Functionals.html "Functionals"



## Bulk File Loading
for a large number of files which may need to be operated on in a similar process and with well structured file names.

firstly a vector of the file names to be accessed must be created, there are multiple options for this:

 

 - Creating the vector manually with `paste0()`

        files <- paste0("file_", 1:100, ".rds")

 - Using `list.files()` with a regex search term for the file type, requires knowledge of regular expressions ([regex][1]) if other files of same type are in the directory.

        files <- list.files("./", pattern = "\\.rds$", full.names = TRUE)
 
where `X` is a vector of part of the files naming format used.

`lapply` will output each response as element of a list.

`readRDS` is specific to `.rds` files and will change depending on the application of the process.
    
    my_file_list <- lapply(files, readRDS)

This is not necessarily faster than a for loop from testing but allows all files to be an element of a list without assigning them explicitly. 

Finally, we often need to load multiple packages at once. 
This trick can do it quite easily by applying `library()` to all libraries that we wish to import:
<!--language:r-lang-->

    lapply(c("jsonlite","stringr","igraph"),library,character.only=TRUE)

  [1]: https://www.wikiod.com/regex/getting-started-with-regular-expressions

## Combining multiple `data.frames` (`lapply`, `mapply`)
In this exercise, we will generate four bootstrap linear regression models and combine the summaries of these models into a single data frame.  

    library(broom)
    
    #* Create the bootstrap data sets
    BootData <- lapply(1:4,
                       function(i) mtcars[sample(1:nrow(mtcars),
                                                 size = nrow(mtcars),
                                                 replace = TRUE), ])
    
    #* Fit the models
    Models <- lapply(BootData,
                     function(BD) lm(mpg ~ qsec + wt + factor(am),
                                     data = BD))
    
    #* Tidy the output into a data.frame
    Tidied <- lapply(Models,
                     tidy)
    
    #* Give each element in the Tidied list a name
    Tidied <- setNames(Tidied, paste0("Boot", seq_along(Tidied)))
    
At this point, we can take two approaches to inserting the names into the data.frame.  

    #* Insert the element name into the summary with `lapply`
    #* Requires passing the names attribute to `lapply` and referencing `Tidied` within
    #* the applied function.
    Described_lapply <- 
     lapply(names(Tidied),
            function(nm) cbind(nm, Tidied[[nm]]))
    
    Combined_lapply <- do.call("rbind", Described_lapply)
    
    #* Insert the element name into the summary with `mapply`
    #* Allows us to pass the names and the elements as separate arguments.
    Described_mapply <- 
     mapply(
      function(nm, dframe) cbind(nm, dframe),
      names(Tidied),
      Tidied,
      SIMPLIFY = FALSE)
    
    Combined_mapply <- do.call("rbind", Described_mapply)

If you're a fan of `magrittr` style pipes, you can accomplish the entire task in a single chain (though it may not be prudent to do so if you need any of the intermediary objects, such as the model objects themselves):

    library(magrittr)
    library(broom)
    Combined <- lapply(1:4,
                       function(i) mtcars[sample(1:nrow(mtcars),
                                                 size = nrow(mtcars),
                                                 replace = TRUE), ]) %>%
     lapply(function(BD) lm( mpg ~ qsec + wt + factor(am), data = BD)) %>%
     lapply(tidy) %>%
     setNames(paste0("Boot", seq_along(.))) %>%
     mapply(function(nm, dframe) cbind(nm, dframe),
            nm = names(.),
            dframe = .,
            SIMPLIFY = FALSE) %>%
     do.call("rbind", .)

## Using user-defined functionals
### User-defined functionals
Users can create their own functionals to varying degrees of complexity. The following examples are from [Functionals][1] by Hadley Wickham:
        
    randomise <- function(f) f(runif(1e3))
            
    lapply2 <- function(x, f, ...) {
        out <- vector("list", length(x))
        for (i in seq_along(x)) {
            out[[i]] <- f(x[[i]], ...)
        }
        out
    }

    
In the first case, `randomise` accepts a single argument `f`, and calls it on a sample of Uniform random variables. To demonstrate equivalence, we call `set.seed` below: 

    set.seed(123)
    randomise(mean)
    #[1] 0.4972778
        
    set.seed(123)
    mean(runif(1e3))
    #[1] 0.4972778
    
    
    set.seed(123)
    randomise(max)
    #[1] 0.9994045
    
    set.seed(123)
    max(runif(1e3))
    #[1] 0.9994045

The second example is a re-implementation of `base::lapply`, which uses functionals to apply an operation (`f`) to each element in a list (`x`). The `...` parameter allows the user to pass additional arguments to `f`, such as the `na.rm` option in the `mean` function: 

    lapply(list(c(1, 3, 5), c(2, NA, 6)), mean)
    # [[1]]
    # [1] 3
    # 
    # [[2]]
    # [1] NA
    
    lapply2(list(c(1, 3, 5), c(2, NA, 6)), mean)
    # [[1]]
    # [1] 3
    # 
    # [[2]]
    # [1] NA
    
    
    lapply(list(c(1, 3, 5), c(2, NA, 6)), mean, na.rm = TRUE)
    # [[1]]
    # [1] 3
    # 
    # [[2]]
    # [1] 4
    
    lapply2(list(c(1, 3, 5), c(2, NA, 6)), mean, na.rm = TRUE)
    # [[1]]
    # [1] 3
    # 
    # [[2]]
    # [1] 4
   

[1]: http://adv-r.had.co.nz/Functionals.html "Functionals"

## Use anonymous functions with apply
`apply` is used to evaluate a function (maybe an anonymous one) over the margins of an array or matrix.

Let's use the `iris` dataset to illustrate this idea. The `iris` dataset has measurements of 150 flowers from 3 species. Let's see how this dataset is structured: 

    > head(iris)

      Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    1         5.1          3.5          1.4         0.2  setosa
    2         4.9          3.0          1.4         0.2  setosa
    3         4.7          3.2          1.3         0.2  setosa
    4         4.6          3.1          1.5         0.2  setosa
    5         5.0          3.6          1.4         0.2  setosa
    6         5.4          3.9          1.7         0.4  setosa

Now, imagine that you want to know the mean of _each_ of these variables. One way to solve this might be to use a `for` loop, but R programmers will often prefer to use `apply` (for reasons why, see Remarks):
     
    > apply(iris[1:4], 2, mean)

    Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
        5.843333     3.057333     3.758000     1.199333

 - In the first parameter, we subset `iris` to include only the first 4 columns, because `mean` only works on numeric data.
 - The second parameter value of `2` indicates that we want to work on the columns only (the second subscript of the r&times;c array); `1` would give the row means.

In the same way we can calculate more meaningful values:

    # standard deviation
    apply(iris[1:4], 2, sd)
    # variance
    apply(iris[1:4], 2, var)

 __Caveat__: R has some built-in functions which are better for calculating column and row sums and means: [`colMeans` and `rowMeans`](http://stat.ethz.ch/R-manual/R-patched/library/base/html/colSums.html).

Now, let's do a different and more meaningful task: let's calculate the mean _only_ for those values which are bigger than `0.5`. For that, we will create our own `mean` function.

    > our.mean.function <- function(x) { mean(x[x > 0.5]) }
    > apply(iris[1:4], 2, our.mean.function)

    Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
        5.843333     3.057333     3.758000     1.665347

*(Note the difference in the mean of `Petal.Width`)*

But, what if we don't want to use this function in the rest of our code? Then, we can use an anonymous function, and write our code like this: 

    apply(iris[1:4], 2, function(x) { mean(x[x > 0.5]) })

So, as we have seen, we can use `apply` to execute the same operation on columns or rows of a dataset using only one line.

**Caveat**: Since `apply` returns very different kinds of output depending on the length of the results of the specified function, it may not be the best choice in cases where you are not working interactively. Some of the other `*apply` family functions are a bit more predictable (see Remarks).

