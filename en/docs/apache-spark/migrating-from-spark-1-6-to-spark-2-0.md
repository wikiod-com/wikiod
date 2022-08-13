---
title: "Migrating from Spark 1.6 to Spark 2.0"
slug: "migrating-from-spark-16-to-spark-20"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

Spark 2.0 has been released and contains many enhancements and new features. If you are using Spark 1.6 and now you want to upgrade your application to use Spark 2.0, you have to take into account some changes in the API. Below are some of the changes to the code that need to be made.

## Update build.sbt file
Update build.sbt with :

    scalaVersion := "2.11.8" // Make sure to have installed Scala 11
    sparkVersion := "2.0.0"  // Make sure to have installed Spark 2.0

Note that when compiling with `sbt package`, the `.jar` will now be created in `target/scala-2.11/`, and the `.jar` name will also be changed, so the `spark-submit` command need to be updated as well.

## Update ML Vector libraries
ML `Transformers` now generates `org.apache.spark.ml.linalg.VectorUDT` instead of `org.apache.spark.mllib.linalg.VectorUDT`.

They are also mapped locally to subclasses of `org.apache.spark.ml.linalg.Vector`. [These are not compatible with old MLLib API which is moving towards deprecation in Spark 2.0.0.][1] 

    //import org.apache.spark.mllib.linalg.{Vector, Vectors} // Depreciated in Spark 2.0 
    import org.apache.spark.ml.linalg.Vector // Use instead


  [1]: http://stackoverflow.com/a/38819323/1575548

