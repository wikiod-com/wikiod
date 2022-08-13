---
title: "Shared Variables"
slug: "shared-variables"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

## Accumulators
Accumulators are write-only variables which can be created with `SparkContext.accumulator`:

    val accumulator = sc.accumulator(0, name = "My accumulator") // name is optional

modified with `+=`:

    val someRDD = sc.parallelize(Array(1, 2, 3, 4))
    someRDD.foreach(element => accumulator += element)

and accessed with `value` method:

    accumulator.value // 'value' is now equal to 10

Using accumulators is complicated by Spark's run-at-least-once guarantee for transformations.  If a transformation needs to be recomputed for any reason, the accumulator updates during that transformation will be repeated. This means that accumulator values may be very different than they would be if tasks had run only once. 
 
---
Note: 
1. Executors *cannot* read accumulator's value. Only the driver program can read the accumulator’s value, using its value method.
2. It is almost similar to counter in Java/MapReduce. So you can relate accumulators to counters to understanding it easily

## Broadcast variables
Broadcast variables are read only shared objects which can be created with `SparkContext.broadcast` method:

    val broadcastVariable = sc.broadcast(Array(1, 2, 3))

and read using `value` method:

    val someRDD = sc.parallelize(Array(1, 2, 3, 4))

    someRDD.map(
        i => broadcastVariable.value.apply(i % broadcastVariable.value.size)
    )

## User Defined Accumulator in Scala
Define `AccumulatorParam`

    import org.apache.spark.AccumulatorParam

    object StringAccumulator extends AccumulatorParam[String] {
      def zero(s: String): String = s
      def addInPlace(s1: String, s2: String)=  s1 + s2
    }

Use:

    val accumulator = sc.accumulator("")(StringAccumulator)
    sc.parallelize(Array("a", "b", "c")).foreach(accumulator += _)


## User Defined Accumulator in Python
Define `AccumulatorParam`:

    from pyspark import AccumulatorParam

    class StringAccumulator(AccumulatorParam):
        def zero(self, s):
            return s
        def addInPlace(self, s1, s2):
            return s1 + s2

    accumulator = sc.accumulator("", StringAccumulator())

    def add(x): 
        global accumulator
        accumulator += x

    sc.parallelize(["a", "b", "c"]).foreach(add)


