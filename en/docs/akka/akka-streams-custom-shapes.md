---
title: "akka-streams custom shapes"
slug: "akka-streams-custom-shapes"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

akka provides some pre-defined shapes, that should probably fit 99.9% of your usage.
creating a new shape should only be done in some very rare cases.
the pre-defined shapes are: 
- `Source` - 1 outlet, no inlets
- `Sink` - 1 inlet, no outlets
- `Flow` - 1 inlet, 1 outlet
- `BidiFlow` - 2 inlets, 2 outlets
- `Closed` - no inlets, no outlets
- `FanInN` - `N` inlets (`N` <= 22), 1 outlet
- `FanOutN` - `N` outlets (`N` <= 22), 1 inlet
- `UniformFanIn` - any number of inlets of the same type, 1 outlet
- `UniformFanOut` - any number of outlets of the same type, 1 inlet
- `Amorphous` - any number of inlets or outlets, but untyped.

## TwoThreeShape
a simple example of how to define a custom shape with 2 inlets and 3 outlets.

<!-- language-all: lang-scala -->

    case class TwoThreeShape[-In1, -In2, +Out1, +Out2, +Out3](
              in1: Inlet[In1@uncheckedVariance],
              in2: Inlet[In2@uncheckedVariance],
              out1: Outlet[Out1@uncheckedVariance],
              out2: Outlet[Out2@uncheckedVariance],
              out3: Outlet[Out3@uncheckedVariance]) extends Shape {

      override val inlets: immutable.Seq[Inlet[_]] = List(in1, in2)
      override val outlets: immutable.Seq[Outlet[_]] = List(out1, out2, out3)

      override def deepCopy(): TwoThreeShape[In1, In2, Out1, Out2, Out3] =
        TwoThreeShape(in1.carbonCopy(), 
                      in2.carbonCopy(), 
                      out1.carbonCopy(), 
                      out2.carbonCopy(), 
                      out3.carbonCopy())

      override def copyFromPorts(inlets: immutable.Seq[Inlet[_]], outlets: immutable.Seq[Outlet[_]]): Shape = {
        require(inlets.size == 2, s"proposed inlets [${inlets.mkString(", ")}] do not fit TwoThreeShape")
        require(outlets.size == 3, s"proposed outlets [${outlets.mkString(", ")}] do not fit TwoThreeShape")
        TwoThreeShape(inlets(0), inlets(1), outlets(0), outlets(1), outlets(2))
      }
    }

an example usage for this weird shape: a stage that will pass through elements of 2 flows, while keeping a ratio of how many elements passed in the flows:

    def ratioCount[X,Y]: Graph[TwoThreeShape[X,Y,X,Y,(Int,Int)],NotUsed] = {
      GraphDSL.create() { implicit b =>
        import GraphDSL.Implicits._
    
        val x = b.add(Broadcast[X](2))
        val y = b.add(Broadcast[Y](2))
        val z = b.add(Zip[Int,Int])
    
        x.out(1).conflateWithSeed(_ => 1)((count,_) => count + 1) ~> z.in0
        y.out(1).conflateWithSeed(_ => 1)((count,_) => count + 1) ~> z.in1
    
        TwoThreeShape(x.in,y.in,x.out(0),y.out(0),z.out)
      }
    }

