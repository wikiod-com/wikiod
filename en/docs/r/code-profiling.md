---
title: "Code profiling"
slug: "code-profiling"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## proc.time()
At its simplest, `proc.time()` gives the total elapsed CPU time in seconds for the current process. Executing it in the console gives the following type of output:

    proc.time()

    #       user     system    elapsed 
    #    284.507    120.397 515029.305 

This is particularly useful for benchmarking specific lines of code. For example:

    t1 <- proc.time()
    fibb <- function (n) {
        if (n < 3) {
            return(c(0,1)[n])
        } else {
            return(fibb(n - 2) + fibb(n -1))
        }
    }
    print("Time one")
    print(proc.time() - t1)

    t2 <- proc.time()
    fibb(30)

    print("Time two")
    print(proc.time() - t2)

This gives the following output:

    source('~/.active-rstudio-document')

    # [1] "Time one"
    #    user  system elapsed 
    #       0       0       0 

    # [1] "Time two"
    #    user  system elapsed 
    #   1.534   0.012   1.572 

`system.time()` is a wrapper for `proc.time()` that returns the elapsed time for a particular command/expression.

    print(t1 <- system.time(replicate(1000,12^2)))
    ##  user  system elapsed 
    ## 0.000   0.000   0.002 

Note that the returned object, of class `proc.time`, is slightly more complicated than it appears on the surface:

    str(t1)
    ## Class 'proc_time'  Named num [1:5] 0 0 0.002 0 0
    ##  ..- attr(*, "names")= chr [1:5] "user.self" "sys.self" "elapsed" "user.child" ...


## Microbenchmark
Microbenchmark is useful for estimating the time taking for otherwise fast procedures. For example, consider estimating the time taken to print hello world. 

    system.time(print("hello world"))

    # [1] "hello world"
    #    user  system elapsed 
    #       0       0       0 

This is because `system.time` is essentially a wrapper function for `proc.time`, which measures in seconds. As printing "hello world" takes less than a second it appears that the time taken is less than a second, however this is not true. To see this we can use the package microbenchmark:

    library(microbenchmark)
    microbenchmark(print("hello world"))
     
    # Unit: microseconds
    #                 expr    min     lq     mean  median     uq     max neval
    # print("hello world") 26.336 29.984 44.11637 44.6835 45.415 158.824   100
    
Here we can see after running `print("hello world")` 100 times, the average time taken was in fact 44 microseconds. (Note that running this code will print "hello world" 100 times onto the console.)

We can compare this against an equivalent procedure, `cat("hello world\n")`, to see if it is faster than `print("hello world")`:

    microbenchmark(cat("hello world\n"))

    # Unit: microseconds
    #                  expr    min      lq     mean median     uq     max neval
    # cat("hello world\\n") 14.093 17.6975 23.73829 19.319 20.996 119.382   100

In this case `cat()` is almost twice as fast as `print()`.

Alternatively one can compare two procedures within the same `microbenchmark` call:

    microbenchmark(print("hello world"), cat("hello world\n"))
    # Unit: microseconds
    # expr                    min     lq     mean  median     uq     max neval
    # print("hello world") 29.122 31.654 39.64255 34.5275 38.852 192.779   100
    # cat("hello world\\n")  9.381 12.356 13.83820 12.9930 13.715  52.564   100



## Benchmarking using microbenchmark


## System.time
System time gives you the CPU time required to execute a R expression, for example:

    system.time(print("hello world"))

    # [1] "hello world"
    #    user  system elapsed 
    #       0       0       0 

You can add larger pieces of code through use of braces:
    
    system.time({
        library(numbers)
        Primes(1,10^5)
    })

Or use it to test functions:

    fibb <- function (n) {   
        if (n < 3) {
            return(c(0,1)[n])
        } else {
            return(fibb(n - 2) + fibb(n -1))
        }
    }

    system.time(fibb(30))



## Line Profiling
One package for line profiling is [lineprof][1] which is written and maintained by Hadley Wickham. Here is a quick demonstration of how it works with `auto.arima` in the forecast package:

    library(lineprof)
    library(forecast)

    l <- lineprof(auto.arima(AirPassengers))
    shine(l)

This will provide you with a shiny app, which allows you to delve deeper into every function call. This enables you to see with ease what is causing your R code to slow down. There is a screenshot of the shiny app below:

[![enter image description here][2]][2]


  [1]: https://github.com/hadley/lineprof
  [2]: http://i.stack.imgur.com/l0hSh.jpg

