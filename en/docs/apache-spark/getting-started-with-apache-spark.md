---
title: "Getting started with apache-spark"
slug: "getting-started-with-apache-spark"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
**Prototype**:

> aggregate(zeroValue, seqOp, combOp)

**Description**: 

`aggregate()` lets you take an RDD and generate a single value that is of a different type than what was stored in the original RDD.

**Parameters**:

 1. `zeroValue`: The initialization value, for your result, in the desired
    format.
 2. `seqOp`: The operation you want to apply to RDD records. Runs once for
    every record in a partition.
 3. `combOp`: Defines how the resulted objects (one for every partition),
    gets combined.

**Example**:

>Compute the sum of a list and the length of that list. Return the result in a pair of `(sum, length)`.

In a Spark shell, create a list with 4 elements, with 2 *partitions*:

    listRDD = sc.parallelize([1,2,3,4], 2)

Then define *seqOp*:

    seqOp = (lambda local_result, list_element: (local_result[0] + list_element, local_result[1] + 1) )

Then define *combOp*:

    combOp = (lambda some_local_result, another_local_result: (some_local_result[0] + another_local_result[0], some_local_result[1] + another_local_result[1]) )

Then aggregated:

    listRDD.aggregate( (0, 0), seqOp, combOp)
    Out[8]: (10, 4)

The first partition has the sublist [1, 2]. This applies the seqOp to each element of that list, which produces a local result - A pair of `(sum, length)` that will reflect the result locally, only in that first partition.

`local_result` gets initialized to the `zeroValue` parameter `aggregate()` was provided with. For example, (0, 0) and `list_element` is the first element of the list:

    0 + 1 = 1
    0 + 1 = 1

The local result is (1, 1), which means the sum is 1 and the length 1 for the 1st partition after processing *only* the first element. `local_result` gets updated from (0, 0), to (1, 1).

    1 + 2 = 3
    1 + 1 = 2

The local result is now (3, 2), which will be the final result from the 1st partition, since they are no other elements in the sublist of the 1st partition. Doing the same for 2nd partition returns (7, 2).

Apply combOp to each local result to form the final, global result:

    (3,2) + (7,2) = (10, 4)

---

Example described in 'figure':

                (0, 0) <-- zeroValue
    
    [1, 2]                  [3, 4]
    
    0 + 1 = 1               0 + 3 = 3
    0 + 1 = 1               0 + 1 = 1
    
    1 + 2 = 3               3 + 4 = 7
    1 + 1 = 2               1 + 1 = 2       
        |                       |
        v                       v
      (3, 2)                  (7, 2)
          \                    / 
           \                  /
            \                /
             \              /
              \            /
               \          / 
               ------------
               |  combOp  |
               ------------
                    |
                    v
                 (10, 4)


  [1]: https://spark.apache.org/docs/1.2.0/api/python/pyspark.html?highlight=aggregate#pyspark.RDD.aggregate
  [2]: http://atlantageek.com/2015/05/30/python-aggregate-rdd/

## Transformation vs Action
Spark uses **lazy evaluation**; that means it will not do any work, unless it really has to. That approach allows us to avoid unnecessary memory usage, thus making us able to work with big data.

A *transformation* is lazy evaluated and the actual work happens, when an *action* occurs.

Example:

    In [1]: lines = sc.textFile(file)        // will run instantly, regardless file's size
    In [2]: errors = lines.filter(lambda line: line.startsWith("error")) // run instantly
    In [3]: errorCount = errors.count()    // an action occurred, let the party start!
    Out[3]: 0                              // no line with 'error', in this example

So, in `[1]` we told Spark to read a file into an RDD, named `lines`. Spark heard us and told us: "Yes I *will* do it", but in fact it didn't *yet* read the file.

In [2], we are filtering the lines of the file, assuming that its contents contain lines with errors that are marked with an `error` in their start. So we tell Spark to create a new RDD, called `errors`, which will have the elements of the RDD `lines`, that had the word `error` at their start.

Now in `[3]`, we ask Spark to *count* the *errors*, i.e. count the number of elements the RDD called `errors` has. `count()` is an **action**, which leave no choice to Spark, but to actually make the operation, so that it can find the result of `count()`, which  will be an integer. 

As a result, when `[3]` is reached, `[1]` and `[2]` will actually being performed, i.e. that when we reach `[3]`, then and only then:

 1. the file is going to be read in `textFile()` (because of `[1]`)

 2. `lines` will be `filter()`'ed (because of `[2]`)

 3. `count()` will execute, because of `[3]`

---

<u>Debug tip</u>: Since Spark won't do any real work until `[3]` is reached, it is important to understand that if an error exist in `[1]` and/or `[2]`, it won't appear, until the action in `[3]` triggers Spark to do actual work. For example if your data in the file do not support the `startsWith()` I used, then `[2]` is going to be properly accepted by Spark and it won't raise any error, but when `[3]` is submitted, and Spark actually evaluates both `[1]` and `[2]`, then and only then it will understand that something is not correct with `[2]` and produce a descriptive error.

As a result, an error may be triggered when `[3]` is executed, but that doesn't mean that the error must lie in the statement of `[3]`!

Note, neither `lines` nor `errors` will be stored in memory after `[3]`. They will continue to exist only as a set of processing instructions. If there will be multiple actions performed on either of these RDDs, spark will read and filter the data multiple times. To avoid duplicating operations when performing multiple actions on a single RDD, it is often useful to store data into memory using `cache`.

---

You can see more transformations/actions in [Spark docs][1].


  [1]: http://spark.apache.org/docs/latest/programming-guide.html#transformations

## Check Spark version
In `spark-shell`:

    sc.version

Generally in a program:

    SparkContext.version

Using `spark-submit`:

     spark-submit --version



