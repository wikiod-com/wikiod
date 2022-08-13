---
title: "Window Functions in Spark SQL"
slug: "window-functions-in-spark-sql"
draft: false
images: []
weight: 9144
type: docs
toc: true
---

## Introduction
Window functions are used to do operations(generally aggregation) on a set of rows collectively called as window. Window functions work in Spark 1.4 or later. Window functions provides more operations then the built-in functions or UDFs, such as substr or round (extensively used before Spark 1.4). Window functions allow users of Spark SQL to calculate results such as the rank of a given row or a moving average over a range of input rows. They significantly improve the expressiveness of Spark’s SQL and DataFrame APIs.

At its core, a window function calculates a return value for every input row of a table based on a group of rows, called the Frame. Every input row can have a unique frame associated with it. This characteristic of window functions makes them more powerful than other functions. The types of window functions are 

 - Ranking functions
 - Analytic functions
 - Aggregate functions

To use window functions, users need to mark that a function is used as a window function by either
 - Adding an `OVER` clause after a supported function in SQL, e.g. `avg(revenue) OVER (...);` or
 - Calling the over method on a supported function in the DataFrame API, e.g. `rank().over(...)`.

This documentation aims to demonstrate some of those functions with example. It is assumed that the reader has some knowledge over basic operations on Spark DataFrame like: adding a new column, renaming a column etc.

**Reading a sample dataset:** 
 

    val sampleData = Seq( ("bob","Developer",125000),("mark","Developer",108000),("carl","Tester",70000),("peter","Developer",185000),("jon","Tester",65000),("roman","Tester",82000),("simon","Developer",98000),("eric","Developer",144000),("carlos","Tester",75000),("henry","Developer",110000)).toDF("Name","Role","Salary")

**List of import statements required:**

    import org.apache.spark.sql.expressions.Window
    import org.apache.spark.sql.functions._
The first statement imports `Window Specification`. A Window Specification contains conditions/specifications indicating, which rows are to be included in the window. 

    scala> sampleData.show
    +------+---------+------+
    |  Name|     Role|Salary|
    +------+---------+------+
    |   bob|Developer|125000|
    |  mark|Developer|108000|
    |  carl|   Tester| 70000|
    | peter|Developer|185000|
    |   jon|   Tester| 65000|
    | roman|   Tester| 82000|
    | simon|Developer| 98000|
    |  eric|Developer|144000|
    |carlos|   Tester| 75000|
    | henry|Developer|110000|
    +------+---------+------+



## Cumulative Sum
To calculate moving average of salary of the employers based on their role:

    val cumSum = sampleData.withColumn("cumulativeSum", sum(sampleData("Salary"))
                 .over( Window.partitionBy("Role").orderBy("Salary")))

 - `orderBy()` sorts salary column and computes cumulative sum.

>     scala> cumSum.show
>     +------+---------+------+-------------+                                         
>     |  Name|     Role|Salary|cumulativeSum|
>     +------+---------+------+-------------+
>     | simon|Developer| 98000|        98000|
>     |  mark|Developer|108000|       206000|
>     | henry|Developer|110000|       316000|
>     |   bob|Developer|125000|       441000|
>     |  eric|Developer|144000|       585000|
>     | peter|Developer|185000|       770000|
>     |   jon|   Tester| 65000|        65000|
>     |  carl|   Tester| 70000|       135000|
>     |carlos|   Tester| 75000|       210000|
>     | roman|   Tester| 82000|       292000|
>     +------+---------+------+-------------+




## Window functions - Sort, Lead, Lag , Rank , Trend Analysis
This topic demonstrates how to use functions like withColumn, lead, lag, Level etc using Spark. Spark dataframe is an sql abstract layer on spark core functionalities. This enable user to write SQL on distributed data. Spark SQL supports hetrogenous file formats including JSON, XML, CSV , TSV etc.

In this blog we have a quick overview of how to use spark SQL and dataframes for common use cases in SQL world.For the sake of simplicity we will deal with a single file which is CSV format. File has four fields, employeeID, employeeName, salary, salaryDate

    1,John,1000,01/01/2016
    1,John,2000,02/01/2016
    1,John,1000,03/01/2016
    1,John,2000,04/01/2016
    1,John,3000,05/01/2016
    1,John,1000,06/01/2016

Save this file as emp.dat.
In the first step we will create a spark dataframe using , spark CSV package from databricks.

    val sqlCont = new HiveContext(sc)
    //Define a schema for file
    val schema = StructType(Array(StructField("EmpId", IntegerType, false),
              StructField("EmpName", StringType, false),
              StructField("Salary", DoubleType, false),
              StructField("SalaryDate", DateType, false)))
    //Apply Shema and read data to a dataframe
    val myDF = sqlCont.read.format("com.databricks.spark.csv")
              .option("header", "false")
              .option("dateFormat", "MM/dd/yyyy")
              .schema(schema)
              .load("src/resources/data/employee_salary.dat")
    //Show dataframe
    myDF.show()

myDF is the dataframe used in remaining excercise. Since myDF is used repeatedly it is recommended to persist it so that it does not need to be reevaluated.


   

     myDF.persist()


Output of dataframe show

    +-----+-------+------+----------+
    |EmpId|EmpName|Salary|SalaryDate|
    +-----+-------+------+----------+
    | 1| John|1000.0|2016-01-01|
    | 1| John|2000.0|2016-02-01|
    | 1| John|1000.0|2016-03-01|
    | 1| John|2000.0|2016-04-01|
    | 1| John|3000.0|2016-05-01|
    | 1| John|1000.0|2016-06-01|
    +-----+-------+------+----------+

**Add a new column to dataframe**

Since spark dataframes are immutable, adding a new column will create a new dataframe with added column. To add a column use withColumn(columnName,Transformation). In below example column empName is formatted to uppercase.

    withColumn(columnName,transformation)
    myDF.withColumn("FormatedName", upper(col("EmpName"))).show()


    +-----+-------+------+----------+------------+
    |EmpId|EmpName|Salary|SalaryDate|FormatedName|
    +-----+-------+------+----------+------------+
    | 1| John|1000.0|2016-01-01| JOHN|
    | 1| John|2000.0|2016-02-01| JOHN|
    | 1| John|1000.0|2016-03-01| JOHN|
    | 1| John|2000.0|2016-04-01| JOHN|
    | 1| John|3000.0|2016-05-01| JOHN|
    | 1| John|1000.0|2016-06-01| JOHN|
    +-----+-------+------+----------+------------+

Sort data based on a column

    val sortedDf = myDF.sort(myDF.col("Salary"))
    sortedDf.show()


    +-----+-------+------+----------+
    |EmpId|EmpName|Salary|SalaryDate|
    +-----+-------+------+----------+
    | 1| John|1000.0|2016-03-01|
    | 1| John|1000.0|2016-06-01|
    | 1| John|1000.0|2016-01-01|
    | 1| John|2000.0|2016-02-01|
    | 1| John|2000.0|2016-04-01|
    | 1| John|3000.0|2016-05-01|
    +-----+-------+------+----------+

**Sort Descending**

desc("Salary")

     myDF.sort(desc("Salary")).show()


    +-----+-------+------+----------+
    |EmpId|EmpName|Salary|SalaryDate|
    +-----+-------+------+----------+
    | 1| John|3000.0|2016-05-01|
    | 1| John|2000.0|2016-02-01|
    | 1| John|2000.0|2016-04-01|
    | 1| John|1000.0|2016-06-01|
    | 1| John|1000.0|2016-01-01|
    | 1| John|1000.0|2016-03-01|
    +-----+-------+------+----------+

**Get and use previous row (Lag)**


LAG is a function in SQL which is used to access previous row values in current row. This is useful when we have use cases like comparison with previous value. LAG in Spark dataframes is available in Window functions

    lag(Column e, int offset)
    Window function: returns the value that is offset rows before the current row, and null if there is less than offset rows before the current row.

 
    import org.apache.spark.sql.expressions.Window
    //order by Salary Date to get previous salary.
    //For first row we will get NULL
    val window = Window.orderBy("SalaryDate")
    //use lag to get previous row value for salary, 1 is the offset
    val lagCol = lag(col("Salary"), 1).over(window)
    myDF.withColumn("LagCol", lagCol).show()

    +-----+-------+------+----------+------+
    |EmpId|EmpName|Salary|SalaryDate|LagCol|
    +-----+-------+------+----------+------+
    | 1| John|1000.0|2016-01-01| null|
    | 1| John|2000.0|2016-02-01|1000.0|
    | 1| John|1000.0|2016-03-01|2000.0|
    | 1| John|2000.0|2016-04-01|1000.0|
    | 1| John|3000.0|2016-05-01|2000.0|
    | 1| John|1000.0|2016-06-01|3000.0|
    +-----+-------+------+----------+------+

**Get and use next row (Lead)**


LEAD is a function in SQL which is used to access next row values in current row. This is useful when we have usecases like comparison with next value. LEAD in Spark dataframes is available in Window functions

    lead(Column e, int offset)
    Window function: returns the value that is offset rows after the current row, and null if there is less than offset rows after the current row.


    import org.apache.spark.sql.expressions.Window
    //order by Salary Date to get previous salary. F
    //or first row we will get NULL
    val window = Window.orderBy("SalaryDate")
    //use lag to get previous row value for salary, 1 is the offset
    val leadCol = lead(col("Salary"), 1).over(window)
    myDF.withColumn("LeadCol", leadCol).show()





    +-----+-------+------+----------+-------+
    |EmpId|EmpName|Salary|SalaryDate|LeadCol|
    +-----+-------+------+----------+-------+
    | 1| John|1000.0|2016-01-01| 1000.0|
    | 1| John|1000.0|2016-03-01| 1000.0|
    | 1| John|1000.0|2016-06-01| 2000.0|
    | 1| John|2000.0|2016-02-01| 2000.0|
    | 1| John|2000.0|2016-04-01| 3000.0|
    | 1| John|3000.0|2016-05-01| null|
    +-----+-------+------+----------+-------+

**Trend analysis with window functions**
Now, let us put window function LAG to use with a simple trend analysis. If salary is less than previous month we will mark it as "DOWN", if salary has increased then "UP". The code use Window function to order by, lag and then do a simple if else with WHEN.

 

       val window = Window.orderBy("SalaryDate")
        //Derive lag column for salary
        val laggingCol = lag(col("Salary"), 1).over(trend_window)
        //Use derived column LastSalary to find difference between current and previous row
        val salaryDifference = col("Salary") - col("LastSalary")
        //Calculate trend based on the difference
        //IF ELSE / CASE can be written using when.otherwise in spark
        val trend = when(col("SalaryDiff").isNull || col("SalaryDiff").===(0), "SAME")
        .when(col("SalaryDiff").>(0), "UP")
        .otherwise("DOWN")
        myDF.withColumn("LastSalary", laggingCol)
        .withColumn("SalaryDiff",salaryDifference)
       .withColumn("Trend", trend).show()

    +-----+-------+------+----------+----------+----------+-----+
    |EmpId|EmpName|Salary|SalaryDate|LastSalary|SalaryDiff|Trend|
    +-----+-------+------+----------+----------+----------+-----+
    | 1| John|1000.0|2016-01-01| null| null| SAME|
    | 1| John|2000.0|2016-02-01| 1000.0| 1000.0| UP|
    | 1| John|1000.0|2016-03-01| 2000.0| -1000.0| DOWN|
    | 1| John|2000.0|2016-04-01| 1000.0| 1000.0| UP|
    | 1| John|3000.0|2016-05-01| 2000.0| 1000.0| UP|
    | 1| John|1000.0|2016-06-01| 3000.0| -2000.0| DOWN|
    +-----+-------+------+----------+----------+----------+-----+












## Moving Average
  To calculate moving average of salary of the employers based on their role:

    val movAvg = sampleData.withColumn("movingAverage", avg(sampleData("Salary"))
                 .over( Window.partitionBy("Role").rowsBetween(-1,1)) )

 - `withColumn()` creates a new column named `movingAverage`, performing `average` on `Salary` column
 - `over()` is used to define window specification.
 - `partitionBy()` partitions the data over the column `Role`
 - `rowsBetween(start, end)` This function defines the rows that are to be included in the window. The parameters (`start` and `end`) takes numerical inputs,`0` represents the current row, `-1` is the previous row, `1` is the next row and so on. The function includes all rows in between `start` and `end`, thus in this example three rows(-1,0,1) are included in the window. 

 

        scala> movAvg.show
    +------+---------+------+------------------+
    |  Name|     Role|Salary|     movingAverage|
    +------+---------+------+------------------+
    |   bob|Developer|125000|          116500.0|
    |  mark|Developer|108000|139333.33333333334|
    | peter|Developer|185000|130333.33333333333|
    | simon|Developer| 98000|142333.33333333334|
    |  eric|Developer|144000|117333.33333333333|
    | henry|Developer|110000|          127000.0|
    |  carl|   Tester| 70000|           67500.0|
    |   jon|   Tester| 65000| 72333.33333333333|
    | roman|   Tester| 82000|           74000.0|
    |carlos|   Tester| 75000|           78500.0|
    +------+---------+------+------------------+



Spark automatically ignores previous and next rows,if the current row is first and last row respectively.

In the above example, movingAverage of first row is average of current & next row only, as previous row doesn't exist. Similarly the last row of the partition (i.e 6th row) is average of current & previous row, as next row doesn't exist. 




