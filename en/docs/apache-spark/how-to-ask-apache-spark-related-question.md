---
title: "How to ask Apache Spark related question?"
slug: "how-to-ask-apache-spark-related-question"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

The goal of this topic is to document best practices when asking Apache Spark related questions.

## Environment details:
When asking Apache Spark related questions please include following information

- Apache Spark version used by the client and Spark deployment if applicable. For API related questions major (1.6, 2.0, 2.1 etc.) is typically sufficient, for questions concerning possible bugs always use full version information.
- Scala version used to build Spark binaries.
- JDK version (`java -version`).
- If you use guest language (Python, R) please provide information about the language version. In Python use tags: [tag:python-2.x], [tag:python-3.x] or more specific ones to distinguish between language variants.
- Build definition (`build.sbt`, `pom.xml`) if applicable or external dependency versions (Python, R) when applicable.
- Cluster manager (`local[n]`, Spark standalone, Yarn, Mesos), mode (`client`, `cluster`) and other submit options if applicable.


## Example data and code
## Example Data 

Please try to provide a minimal example input data in a format that can be directly used by the answers without tedious and time consuming parsing for example input file or local collection with all code required to create distributed data structures. 

When applicable always include type information:

- In RDD based API use type annotations when necessary.
- In DataFrame based API provide schema information as a `StrucType` or output from `Dataset.printSchema`.

Output from `Dataset.show` or `print` can look good but doesn't tell us anything about underlying types.

If particular problem occurs only at scale use random data generators (Spark provides some useful utilities in `org.apache.spark.mllib.random.RandomRDDs` and `org.apache.spark.graphx.util.GraphGenerators`


## Code

Please use type annotations when possible. While your compiler can easily keep track of the types it is not so easy for mere mortals. For example:

    val lines: RDD[String] = rdd.map(someFunction)

or

    def f(x: String): Int = ???

are better than:

    val lines = rdd.map(someFunction)

and

    def f(x: String) = ???

respectively.




## Diagnostic information
## Debugging questions.

When question is related to debugging specific exception always provide relevant traceback. While it is advisable to remove duplicated outputs (from different executors or attempts) don't cut tracebacks to a single line or exception class only. 

## Performance questions.

Depending on the context try to provide details like:

- `RDD.debugString` / `Dataset.explain`.
- Output from Spark UI with DAG diagram if applicable in particular case.
- Relevant log messages.
- Diagnostic information collected by external tools (Ganglia, VisualVM).

## Before you ask
- Search Stack Overflow for duplicate questions. There common class of problems which have been already extensively documented.
- Read [How do I ask a good question?](https://stackoverflow.com/help/how-to-ask).
- Read [What topics can I ask about here?](https://stackoverflow.com/help/on-topic)
- [Apache Spark Community resources](https://spark.apache.org/community.html)


