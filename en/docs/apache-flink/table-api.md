---
title: "Table API"
slug: "table-api"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Maven dependencies
To use the Table API, add `flink-table` as a maven dependency (in addition to `flink-clients` and `flink-core`):

<!-- language: lang-xml -->

    <dependency>
        <groupId>org.apache.flink</groupId>
        <artifactId>flink-table_2.11</artifactId>
        <version>1.1.4</version>
    </dependency>

Ensure that the scala version (here 2.11) is compatible with your system.

## Simple aggregation from a CSV
Given the CSV file `peoples.csv`:

    1,Reed,United States,Female
    2,Bradley,United States,Female
    3,Adams,United States,Male
    4,Lane,United States,Male
    5,Marshall,United States,Female
    6,Garza,United States,Male
    7,Gutierrez,United States,Male
    8,Fox,Germany,Female
    9,Medina,United States,Male
    10,Nichols,United States,Male
    11,Woods,United States,Male
    12,Welch,United States,Female
    13,Burke,United States,Female
    14,Russell,United States,Female
    15,Burton,United States,Male
    16,Johnson,United States,Female
    17,Flores,United States,Male
    18,Boyd,United States,Male
    19,Evans,Germany,Male
    20,Stephens,United States,Male

We want to count people by country and by country+gender:

<!-- language: lang-java -->

    public class TableExample{
        public static void main( String[] args ) throws Exception{
            // create the environments
            final ExecutionEnvironment env = ExecutionEnvironment.getExecutionEnvironment();
            final BatchTableEnvironment tableEnv = TableEnvironment.getTableEnvironment( env );
    
            // get the path to the file in resources folder
            String peoplesPath = TableExample.class.getClassLoader().getResource( "peoples.csv" ).getPath();
            // load the csv into a table
            CsvTableSource tableSource = new CsvTableSource(
                    peoplesPath,
                    "id,last_name,country,gender".split( "," ),
                    new TypeInformation[]{ Types.INT(), Types.STRING(), Types.STRING(), Types.STRING() } );
            // register the table and scan it
            tableEnv.registerTableSource( "peoples", tableSource );
            Table peoples = tableEnv.scan( "peoples" );
    
            // aggregation using chain of methods
            Table countriesCount = peoples.groupBy( "country" ).select( "country, id.count" );
            DataSet<Row> result1 = tableEnv.toDataSet( countriesCount, Row.class );
            result1.print();
    
            // aggregation using SQL syntax
            Table countriesAndGenderCount = tableEnv.sql(
                    "select country, gender, count(id) from peoples group by country, gender" );
    
            DataSet<Row> result2 = tableEnv.toDataSet( countriesAndGenderCount, Row.class );
            result2.print();
        }
    }

The results are:

    Germany,2
    United States,18

    Germany,Male,1
    United States,Male,11
    Germany,Female,1
    United States,Female,7


## Join tables example

In addition to `peoples.csv` (see _simple aggregation from a CSV_) we have two more CSVs representing products and sales.

`sales.csv` (people_id, product_id):

    19,5
    6,4
    10,4
    2,4
    8,1
    19,2
    8,4
    5,5
    13,5
    4,4
    6,1
    3,3
    8,3
    17,2
    6,2
    1,2
    3,5
    15,5
    3,3
    6,3
    13,2
    20,4
    20,2

`products.csv` (id, name, price):

    1,Loperamide,47.29
    2,pain relief pm,61.01
    3,Citalopram,48.13
    4,CTx4 Gel 5000,12.65
    5,Namenda,27.67


We want to get the name and product for each sale of more than 40$:

<!-- language: lang-java -->

    public class SimpleJoinExample{
        public static void main( String[] args ) throws Exception{

            final ExecutionEnvironment env = ExecutionEnvironment.getExecutionEnvironment();
            final BatchTableEnvironment tableEnv = TableEnvironment.getTableEnvironment( env );

            String peoplesPath = TableExample.class.getClassLoader().getResource( "peoples.csv" ).getPath();
            String productsPath = TableExample.class.getClassLoader().getResource( "products.csv" ).getPath();
            String salesPath = TableExample.class.getClassLoader().getResource( "sales.csv" ).getPath();

            Table peoples = csvTable(
                    tableEnv,
                    "peoples",
                    peoplesPath,
                    "pe_id,last_name,country,gender",
                    new TypeInformation[]{ Types.INT(), Types.STRING(), Types.STRING(), Types.STRING() } );

            Table products = csvTable(
                    tableEnv,
                    "products",
                    productsPath,
                    "prod_id,product_name,price",
                    new TypeInformation[]{ Types.INT(), Types.STRING(), Types.FLOAT() } );

            Table sales = csvTable(
                    tableEnv,
                    "sales",
                    salesPath,
                    "people_id,product_id",
                    new TypeInformation[]{ Types.INT(), Types.INT() } );

            // here is the interesting part:
            Table join = peoples
                    .join( sales ).where( "pe_id = people_id" )
                    .join( products ).where( "product_id = prod_id" )
                    .select( "last_name, product_name, price" )
                    .where( "price < 40" );

            DataSet<Row> result = tableEnv.toDataSet( join, Row.class );
            result.print();

        }//end main


        public static Table csvTable( BatchTableEnvironment tableEnv, String name, String path, String header,
                                      TypeInformation[]
                                              typeInfo ){
            CsvTableSource tableSource = new CsvTableSource( path, header.split( "," ), typeInfo);
            tableEnv.registerTableSource( name, tableSource );
            return tableEnv.scan( name );
        }

    }//end class

Note that it is important to use different names for each column, otherwise flink will complain about "ambiguous names in join".

Result:

    Burton,Namenda,27.67
    Marshall,Namenda,27.67
    Burke,Namenda,27.67
    Adams,Namenda,27.67
    Evans,Namenda,27.67
    Garza,CTx4 Gel 5000,12.65
    Fox,CTx4 Gel 5000,12.65
    Nichols,CTx4 Gel 5000,12.65
    Stephens,CTx4 Gel 5000,12.65
    Bradley,CTx4 Gel 5000,12.65
    Lane,CTx4 Gel 5000,12.65


## Using external sinks
A Table can be written to a TableSink, which is a generic interface to support different formats and file systems. A batch Table can only be written to a `BatchTableSink`, while a streaming table requires a `StreamTableSink`. 

Currently, flink offers only the `CsvTableSink` interface. 

## Usage

In the examples above, replace: 

<!-- language: lang-java -->

    DataSet<Row> result = tableEnv.toDataSet( table, Row.class );
    result.print();

with: 

<!-- language: lang-java -->

    TableSink sink = new CsvTableSink("/tmp/results", ",");
    // write the result Table to the TableSink
    table.writeToSink(sink);
    // start the job
    env.execute();

`/tmp/results` is a folder, because flink does parallel operations. Hence, if you have 4 processors, you will likely have 4 files in the results folder. 

Also, note that we explicitely call `env.execute()`: this is necessary to start a flink job, but in the previous examples `print()` did it for us.

