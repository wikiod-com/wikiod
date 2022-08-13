---
title: "Stateful operations in Spark Streaming"
slug: "stateful-operations-in-spark-streaming"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

## PairDStreamFunctions.updateStateByKey
`updateState` by key can be used to create a stateful `DStream` based on upcoming data. It requires a function:

<!-- language: scala -->

    object UpdateStateFunctions {
      def updateState(current: Seq[Double], previous: Option[StatCounter]) = {
        previous.map(s => s.merge(current)).orElse(Some(StatCounter(current)))
      }
    }

which takes a sequence of the `current` values, an `Option` of previous state and returns an `Option` of the updated state. Putting this all together:

<!-- language: scala -->


    import org.apache.spark._
    import org.apache.spark.streaming.dstream.DStream
    import scala.collection.mutable.Queue
    import org.apache.spark.util.StatCounter
    import org.apache.spark.streaming._

    object UpdateStateByKeyApp {
      def main(args: Array[String]) {

        val sc = new SparkContext("local", "updateStateByKey", new SparkConf())
        val ssc = new StreamingContext(sc, Seconds(10))
        ssc.checkpoint("/tmp/chk")

        val queue = Queue(
          sc.parallelize(Seq(("foo", 5.0), ("bar", 1.0))),
          sc.parallelize(Seq(("foo", 1.0), ("foo", 99.0))),
          sc.parallelize(Seq(("bar", 22.0), ("foo", 1.0))),
          sc.emptyRDD[(String, Double)],
          sc.emptyRDD[(String, Double)],
          sc.emptyRDD[(String, Double)],
          sc.parallelize(Seq(("foo", 1.0), ("bar", 1.0)))
        )

        val inputStream: DStream[(String, Double)] = ssc.queueStream(queue)

        inputStream.updateStateByKey(UpdateStateFunctions.updateState _).print()

        ssc.start()
        ssc.awaitTermination()
        ssc.stop()
      }
    }

## PairDStreamFunctions.mapWithState
`mapWithState`, similarly to `updateState`, can be used to create a stateful DStream based on upcoming data. It requires `StateSpec`:


<!-- language: scala -->

    import org.apache.spark.streaming._

    object StatefulStats {
      val state = StateSpec.function(
        (key: String, current: Option[Double], state: State[StatCounter]) => {
          (current, state.getOption) match {
            case (Some(x), Some(cnt)) => state.update(cnt.merge(x))
            case (Some(x), None) => state.update(StatCounter(x))
            case (None, None) => state.update(StatCounter())
            case _ =>
          }
          
          (key, state.get)
        }
      )
    }

which takes key `key`, current `value` and accumulated `State` and returns new state. Putting this all together:

<!-- language: scala -->


    import org.apache.spark._
    import org.apache.spark.streaming.dstream.DStream
    import scala.collection.mutable.Queue
    import org.apache.spark.util.StatCounter

    object MapStateByKeyApp {
      def main(args: Array[String]) {
        val sc = new SparkContext("local", "mapWithState", new SparkConf())

        val ssc = new StreamingContext(sc, Seconds(10))
        ssc.checkpoint("/tmp/chk")

        val queue = Queue(
          sc.parallelize(Seq(("foo", 5.0), ("bar", 1.0))),
          sc.parallelize(Seq(("foo", 1.0), ("foo", 99.0))),
          sc.parallelize(Seq(("bar", 22.0), ("foo", 1.0))),
          sc.emptyRDD[(String, Double)],
          sc.parallelize(Seq(("foo", 1.0), ("bar", 1.0)))
        )

        val inputStream: DStream[(String, Double)] = ssc.queueStream(queue)

        inputStream.mapWithState(StatefulStats.state).print()

        ssc.start()
        ssc.awaitTermination()
        ssc.stop()
      }
    }

Finally expected output:

    -------------------------------------------
    Time: 1469923280000 ms
    -------------------------------------------
    (foo,(count: 1, mean: 5.000000, stdev: 0.000000, max: 5.000000, min: 5.000000))
    (bar,(count: 1, mean: 1.000000, stdev: 0.000000, max: 1.000000, min: 1.000000))
    
    -------------------------------------------
    Time: 1469923290000 ms
    -------------------------------------------
    (foo,(count: 3, mean: 35.000000, stdev: 45.284287, max: 99.000000, min: 1.000000))
    (foo,(count: 3, mean: 35.000000, stdev: 45.284287, max: 99.000000, min: 1.000000))
    
    -------------------------------------------
    Time: 1469923300000 ms
    -------------------------------------------
    (bar,(count: 2, mean: 11.500000, stdev: 10.500000, max: 22.000000, min: 1.000000))
    (foo,(count: 4, mean: 26.500000, stdev: 41.889736, max: 99.000000, min: 1.000000))
    
    -------------------------------------------
    Time: 1469923310000 ms
    -------------------------------------------
    
    -------------------------------------------
    Time: 1469923320000 ms
    -------------------------------------------
    (foo,(count: 5, mean: 21.400000, stdev: 38.830916, max: 99.000000, min: 1.000000))
    (bar,(count: 3, mean: 8.000000, stdev: 9.899495, max: 22.000000, min: 1.000000))

