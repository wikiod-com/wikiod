---
title: "Hive User Defined Functions (UDF's)"
slug: "hive-user-defined-functions-udfs"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

## Hive UDF creation

To create a UDF, we need to extend UDF (`org.apache.hadoop.hive.ql.exec.UDF`) class and implement evaluate method.

Once UDF is complied and JAR is build, we need to add jar to hive context to create a temporary/permanent function.


    import org.apache.hadoop.hive.ql.exec.UDF;    

    class UDFExample extends UDF {
      
      public String evaluate(String input) {
        
        return new String("Hello " + input);
      }
    }

    hive> ADD JAR <JAR NAME>.jar;
    hive> CREATE TEMPORARY FUNCTION helloworld as 'package.name.UDFExample';
    hive> select helloworld(name) from test;

## Hive UDF to trim the given string.
    package MyHiveUDFs;
    
    import org.apache.commons.lang.StringUtils;
    import org.apache.hadoop.hive.ql.exec.UDF;
    import org.apache.hadoop.io.Text;

    public class Strip extends UDF {

    private Text result = new Text();
     public Text evaluate(Text str) {
     if(str == null) {
     return null;
     }
     result.set(StringUtils.strip(str.toString()));
     return result;
     }
    }

export the above to jar file

Go to the Hive CLI and Add the UDF JAR

    hive> ADD jar /home/cloudera/Hive/hive_udf_trim.jar;

Verify JAR is in Hive CLI Classpath

    hive> list jars;
    /home/cloudera/Hive/hive_udf_trim.jar

Create Temporary Function

    hive> CREATE TEMPORARY FUNCTION STRIP AS 'MyHiveUDFs.Strip';

UDF Output

     hive> select strip('   hiveUDF ') from dummy;
     OK
     hiveUDF

   





