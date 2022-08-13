---
title: "Getting started with Machine Learning using Apache spark MLib"
slug: "getting-started-with-machine-learning-using-apache-spark-mlib"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Apache spark MLib provides (JAVA, R, PYTHON, SCALA)
1.) Various Machine learning algorithms on regression, classification, clustering, collaborative filtering which are mostly used approaches in Machine learning.
2.) It supports feature extraction, transformation etc.
3.) It allows data practitioners to solve their machine learning problems (as well  as graph computation, streaming, and real-time interactive query processing) interactively and at much greater scale.
 

Please refer below given to know more about spark MLib

 1. http://spark.apache.org/docs/latest/ml-guide.html
 2. https://mapr.com/ebooks/spark/

## Write your first classification problem using Logistic Regression model
I am using eclipse here, and you need to add below given dependency to your pom.xml
  
  1.) **POM.XML**

        <project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
      <modelVersion>4.0.0</modelVersion>
    
      <groupId>com.predection.classification</groupId>
      <artifactId>logisitcRegression</artifactId>
      <version>0.0.1-SNAPSHOT</version>
      <packaging>jar</packaging>
    
      <name>logisitcRegression</name>
      <url>http://maven.apache.org</url>
    
      <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
      </properties>
    
      <dependencies>
         <!-- Spark -->
            <dependency>
                <groupId>org.apache.spark</groupId>
                <artifactId>spark-core_2.11</artifactId>
                <version>2.1.0</version>
            </dependency>
            <dependency>
                <groupId>org.apache.spark</groupId>
                <artifactId>spark-mllib_2.10</artifactId>
                <version>2.1.0</version>
            </dependency>
            <dependency>
                <groupId>org.apache.spark</groupId>
                <artifactId>spark-sql_2.11</artifactId>
                <version>2.1.0</version>
            </dependency>
        </dependencies>
    </project>

<br>
2.) APP.JAVA(your application class)<br><br>

   

We are doing classification based on country, hours and our label is clicked.
    
        package com.predection.classification.logisitcRegression;
    
    import org.apache.spark.SparkConf;
    import org.apache.spark.ml.classification.LogisticRegression;
    import org.apache.spark.ml.classification.LogisticRegressionModel;
    import org.apache.spark.ml.feature.StringIndexer;
    import org.apache.spark.ml.feature.VectorAssembler;
    import org.apache.spark.sql.Dataset;
    import org.apache.spark.sql.Row;
    import org.apache.spark.sql.SparkSession;
    import org.apache.spark.sql.types.StructField;
    import org.apache.spark.sql.types.StructType;
    import java.util.Arrays;
    import java.util.List;
    import org.apache.spark.sql.RowFactory;
    import static org.apache.spark.sql.types.DataTypes.*;
    
    /**
     * Classification problem using Logistic Regression Model
     *
     */

    public class App 
    {
        public static void main( String[] args )
        {
            SparkConf sparkConf = new SparkConf().setAppName("JavaLogisticRegressionExample");
            
            // Creating spark session
            SparkSession sparkSession = SparkSession.builder().config(sparkConf).getOrCreate();
            
            StructType schema = createStructType(new StructField[]{
                      createStructField("id", IntegerType, false),
                      createStructField("country", StringType, false),
                      createStructField("hour", IntegerType, false),
                      createStructField("clicked", DoubleType, false)
                    });
    
                    List<Row> data = Arrays.asList(
                      RowFactory.create(7, "US", 18, 1.0),
                      RowFactory.create(8, "CA", 12, 0.0),
                      RowFactory.create(9, "NZ", 15, 1.0),
                

 

     RowFactory.create(10,"FR", 8, 0.0),
                      RowFactory.create(11, "IT", 16, 1.0),
                  RowFactory.create(12, "CH", 5, 0.0),
                  RowFactory.create(13, "AU", 20, 1.0)
                );
    
            
    Dataset<Row> dataset = sparkSession.createDataFrame(data, schema);        
    
    // Using stringindexer transformer to transform string into index
     dataset = new StringIndexer().setInputCol("country").setOutputCol("countryIndex").fit(dataset).transform(dataset);
     
    // creating feature vector using dependent variables countryIndex, hours are features and clicked is label
    VectorAssembler assembler = new VectorAssembler()
            .setInputCols(new String[] {"countryIndex", "hour"})
            .setOutputCol("features");
    
        Dataset<Row> finalDS = assembler.transform(dataset);
        
        // Split the data into training and test sets (30% held out for
        // testing).
            Dataset<Row>[] splits = finalDS.randomSplit(new double[] { 0.7, 0.3 });
            Dataset<Row> trainingData = splits[0];
            Dataset<Row> testData = splits[1];
            trainingData.show();
            testData.show();
            // Building LogisticRegression Model
            LogisticRegression lr = new LogisticRegression().setMaxIter(10).setRegParam(0.3).setElasticNetParam(0.8).setLabelCol("clicked");
    
            // Fit the model
            LogisticRegressionModel lrModel = lr.fit(trainingData);
            
            // Transform the model, and predict class for test dataset
            Dataset<Row> output = lrModel.transform(testData);
            output.show();
        }
    }


3.) To run this application, first perform `mvn-clean-package` on application project, it would create jar.
4.) Open spark root directory, and submit this job

    bin/spark-submit --class com.predection.regression.App --master local[2] ./regression-0.0.1-SNAPSHOT.jar(path to the jar file)

5.) After submitting see it builds training data

[![enter image description here][1]][1]

  
6.) same way test data

[![enter image description here][2]][2]

7.) And here is the prediction result under the prediction column 

[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/qKU1K.png
  [2]: https://i.stack.imgur.com/rdUhx.png
  [3]: https://i.stack.imgur.com/WMtUq.png


