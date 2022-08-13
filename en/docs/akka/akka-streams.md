---
title: "Akka Streams"
slug: "akka-streams"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Akka Streams: Hello World
Akka Streams allows you to easily create a stream leveraging the power of the Akka framework without explicitly defining actor behaviors and messages. Every stream will have at least one `Source` (origin of the data) and at least one `Sink` (destination of the data). 

    import akka.actor.ActorSystem
    import akka.stream.ActorMaterializer
    import akka.stream.scaladsl.{Sink, Source}
    import java.io.File

    val stream = Source(Seq("test1.txt", "test2.txt", "test3.txt"))
      .map(new File(_))
      .filter(_.exists())
      .filter(_.length() != 0)
      .to(Sink.foreach(f => println(s"Absolute path: ${f.getAbsolutePath}")))

In this quick example we have a `Seq` of filenames that we input into the stream. First we map them to a `File`, then we filter out files which don't exist, then files which length is 0. If a file went through the filters, it gets printed into the `stdout`.

Akka streams also allows you to do streams in a modular way. You can create `Flow`s with the partial modules of your stream. If we take the same example we could also do: 

    import akka.actor.ActorSystem
    import akka.stream.ActorMaterializer
    import akka.stream.scaladsl.{Sink, Source}
    import java.io.File

    implicit val actorSystem = ActorSystem("system")
    implicit val actorMaterializer = ActorMaterializer()

    val source = Source(List("test1.txt", "test2.txt", "test3.txt"))
    val mapper = Flow[String].map(new File(_))
    val existsFilter = Flow[File].filter(_.exists())
    val lengthZeroFilter = Flow[File].filter(_.length() != 0)
    val sink = Sink.foreach[File](f => println(s"Absolute path: ${f.getAbsolutePath}"))

    val stream = source
      .via(mapper)
      .via(existsFilter)
      .via(lengthZeroFilter)
      .to(sink)

    stream.run()

In this second version we can see that `mapper`, `existsFilter`, `lengthZeroFilter` are `Flow`s. You can compose them in stream by using the method `via`. This capability would allow you to reuse your pieces of code. One important thing to mention is that `Flow`s can be stateless or stateful. In the case of stateful, you need to be careful when reusing them.

You can also think about streams as `Graphs`. Akka Streams also provides a powerful `GraphDSL` to define complicated streams in a simple way. Following with the same example we could do:

    import java.io.File
    import akka.actor.ActorSystem
    import akka.stream.{ActorMaterializer, ClosedShape}
    import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Sink, Source}

    implicit val actorSystem = ActorSystem("system")
    implicit val actorMaterializer = ActorMaterializer()

    val graph = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      val source = Source(List("test1.txt", "test2.txt", "test3.txt"))
      val mapper = Flow[String].map(new File(_))
      val existsFilter = Flow[File].filter(_.exists())
      val lengthZeroFilter = Flow[File].filter(_.length() != 0)
      val sink = Sink.foreach[File](f => println(s"Absolute path: ${f.getAbsolutePath}"))

      source ~> mapper ~> existsFilter ~> lengthZeroFilter ~> sink

      ClosedShape
    })

    graph.run()

It is also possible to create aggregated flow using the `GraphDSL`. For example, if we would like to combine the mapper and two filters in one we could do:

    val combinedFlow = Flow.fromGraph(GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val mapper = builder.add(Flow[String].map(new File(_)))
      val existsFilter = builder.add(Flow[File].filter(_.exists()))
      val lengthZeroFilter = builder.add(Flow[File].filter(_.length() != 0))

      mapper ~> existsFilter ~> lengthZeroFilter

      FlowShape(mapper.in, lengthZeroFilter.out)
    })

And then use it as a individual block. `combinedFlow` would be a `FlowShape` or a `PartialGraph`. We can us for example with `via`: 

    val stream = source
      .via(combinedFlow)
      .to(sink)

    stream.run()

Or using the `GraphDSL`:

    val graph = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      val source = Source(List("test1.txt", "test2.txt", "test3.txt"))
      val sink = Sink.foreach[File](f => println(s"Absolute path: ${f.getAbsolutePath}"))

      source ~> combinedFlow ~> sink

      ClosedShape
    })

    graph.run()

## Akka-Streams: subflows
You can dynamically fork a flow in multiple subflows using `groupBy`. The continuing stages are applied to each subflow until you merge them back using `mergeSubstreams`.

    val sumByKey: Flow[(String, Int), Int, NotUsed] = 
      Flow[(String, Int)].
        groupBy(Int.maxValue, _._1).  //forks the flow
        map(_._2).                    //this is applied to each subflow
        fold(0)(_ + _).
        mergeSubstreams               //the subflow outputs are merged back together

