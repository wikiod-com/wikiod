---
title: "Handling JSON in Spark"
slug: "handling-json-in-spark"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Mapping JSON to a Custom Class with Gson
With [`Gson`][1], you can read JSON dataset and map them to a custom class `MyClass`.

Since [`Gson`][2] is not serializable, each executor needs its own [`Gson`][3] object. Also, `MyClass` must be serializable in order to pass it between executors.

Note that the file(s) that is offered as a json file is not a typical JSON file. Each line must contain a separate, self-contained valid JSON object. As a consequence, a regular multi-line JSON file will most often fail.

<!-- language: scala -->

    val sc: org.apache.spark.SparkContext // An existing SparkContext

    // A JSON dataset is pointed to by path.
    // The path can be either a single text file or a directory storing text files.
    val path = "path/to/my_class.json"
    val linesRdd: RDD[String] = sc.textFile(path)

    // Mapping json to MyClass
    val myClassRdd: RDD[MyClass] = linesRdd.map{ l => 
        val gson = new com.google.gson.Gson()
        gson.fromJson(l, classOf[MyClass])
    }

If creation of [`Gson`][3] object becomes too costly, [`mapPartitions`][4] method can be used to optimize it. With it, there will be one [`Gson`][3] per partition instead of per line:

<!-- language: scala -->

    val myClassRdd: RDD[MyClass] = linesRdd.mapPartitions{p => 
        val gson = new com.google.gson.Gson()
        p.map(l => gson.fromJson(l, classOf[MyClass]))
    }


  [1]: https://github.com/google/gson/blob/master/UserGuide.md
  [2]: https://github.com/google/gson/blob/master/gson/src/main/java/com/google/gson/Gson.java#L103
  [3]: https://google.github.io/gson/apidocs/com/google/gson/Gson.html
  [4]: http://spark.apache.org/docs/latest/api/scala/index.html#org.apache.spark.rdd.RDD

