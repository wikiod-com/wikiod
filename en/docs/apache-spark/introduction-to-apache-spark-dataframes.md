---
title: "Introduction to Apache Spark DataFrames"
slug: "introduction-to-apache-spark-dataframes"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Spark Dataframe explained
In Spark, a DataFrame is a distributed collection of data organized into named columns. It is conceptually equivalent to a table in a relational database or a data frame in R/Python, but with richer optimizations under the hood. DataFrames can be constructed from a wide array of sources such as structured data files, tables in Hive, external databases, or existing RDDs.

**Ways of creating Dataframe**

    val data= spark.read.json("path to json")

`val df = spark.read.format("com.databricks.spark.csv").load("test.txt")`
in the options field, you can provide header, delimiter, charset and much more

you can also create Dataframe from an RDD

    val rdd = sc.parallelize(
      Seq(
        ("first", Array(2.0, 1.0, 2.1, 5.4)),
        ("test", Array(1.5, 0.5, 0.9, 3.7)),
        ("choose", Array(8.0, 2.9, 9.1, 2.5))
      )
    )

    val dfWithoutSchema = spark.createDataFrame(rdd)

If you want to create df with schema 

    def createDataFrame(rowRDD: RDD[Row], schema: StructType): DataFrame


**Why we need Dataframe if Spark has provided RDD**

An RDD is merely a Resilient Distributed Dataset that is more of a blackbox of data that cannot be optimized as the operations that can be performed against it, are not as constrained.

No inbuilt optimization engine: When working with structured data, RDDs cannot take advantages of Spark’s advanced optimizers including catalyst optimizer and Tungsten execution engine. Developers need to optimize each RDD based on its attributes.
Handling structured data: Unlike Dataframe and datasets, RDDs don’t infer the schema of the ingested data and requires the user to specify it.

DataFrames in Spark have their execution automatically optimized by a query optimizer. Before any computation on a DataFrame starts, the Catalyst optimizer compiles the operations that were used to build the DataFrame into a physical plan for execution. Because the optimizer understands the semantics of operations and structure of the data, it can make intelligent decisions to speed up computation.

**Limitation of DataFrame**

Compile-time type safety: Dataframe API does not support compile time safety which limits you from manipulating data when the structure is not known.




## Spark DataFrames with JAVA
A DataFrame is a distributed collection of data organized into named columns. It is conceptually equivalent to a table in a relational database. DataFrames can be constructed from a wide array of sources such as: structured data files, tables in Hive, external databases, or existing RDDs.

Reading a Oracle RDBMS table into spark data frame::

    SparkConf sparkConf = new SparkConf().setAppName("SparkConsumer");

    sparkConf.registerKryoClasses(new Class<?>[]{  
            Class.forName("org.apache.hadoop.io.Text"),
            Class.forName("packageName.className")
    });

    JavaSparkContext sparkContext=new JavaSparkContext(sparkConf);
    SQLContext sqlcontext= new SQLContext(sparkContext);
    
    Map<String, String> options = new HashMap();
    options.put("driver", "oracle.jdbc.driver.OracleDriver");
    options.put("url", "jdbc:oracle:thin:username/password@host:port:orcl"); //oracle url to connect
    options.put("dbtable", "DbName.tableName");
    DataFrame df=sqlcontext.load("jdbc", options);
    df.show(); //this will print content into tablular format

We can also convert this data frame back to rdd if need be :

 

    JavaRDD<Row> rdd=df.javaRDD();

 
 
 Create a dataframe from a file:
 
 

    public class LoadSaveTextFile {
    
        //static schema class
        public static class Schema implements Serializable {
    
            public String getTimestamp() {
                return timestamp;
            }
            public void setTimestamp(String timestamp) {
                this.timestamp = timestamp;
            }
            public String getMachId() {
                return machId;
            }
            public void setMachId(String machId) {
                this.machId = machId;
            }
            public String getSensorType() {
                return sensorType;
            }
            public void setSensorType(String sensorType) {
                this.sensorType = sensorType;
            }    
            
            //instance variables
            private String timestamp;
            private String machId;
            private String sensorType;
        }
        
        public static void main(String[] args) throws ClassNotFoundException {
    
            SparkConf sparkConf = new SparkConf().setAppName("SparkConsumer");
    
            sparkConf.registerKryoClasses(new Class<?>[]{  
                    Class.forName("org.apache.hadoop.io.Text"),
                    Class.forName("oracle.table.join.LoadSaveTextFile")
            });
    
            JavaSparkContext sparkContext=new JavaSparkContext(sparkConf);
            SQLContext sqlcontext= new SQLContext(sparkContext);
    
            //we have a file which ";" separated 
            String filePath=args[0];
        
            JavaRDD<Schema> schemaRdd = sparkContext.textFile(filePath).map(
                    new Function<String, Schema>() {
                        public Schema call(String line) throws Exception {
                            String[] tokens=line.split(";");
                            Schema schema = new Schema();
                            schema.setMachId(tokens[0]);
                            schema.setSensorType(tokens[1]);
                            schema.setTimestamp(tokens[2]);
                            return schema;
                        }
                    });
    
            DataFrame df = sqlcontext.createDataFrame(schemaRdd, Schema.class);
            df.show();
        }   
    }

Now we have data frame from oracle as well from a file. Similarly we can read a table from hive as well. On data frame we can fetch any column as we do in rdbms. Like get a min value for a column or max value. Can calculate a mean/avg for a column. Some other functions like select,filter,agg, groupBy are also available.

