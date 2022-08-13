---
title: "Getting started with system.reactive"
slug: "getting-started-with-systemreactive"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Using Rx in your project
Install the NuGet package `System.Reactive`, then add this using statement to access the Rx extension methods:

    using System.Reactive.Linq;

## Wrapping an async method as an observable


## Sharing a single subscription (Publish + RefCount)


## Ignoring repeated values


## Filtering the values of an observable


## Selecting a new value for each value in an observable


## Subscribing to an observable (CancellationToken)


## Sharing a single subscription (Publish)


## Subscribing/unsubscribing to an observable (IDisposable)


## Installation or Setup
Reactive Extensions are published on both [NuGet][1] and [MyGet][2].

Installing and using them is therefore the same as any other NuGet package:

     Install-Package System.Reactive

NB package names changed between v2 and v3. [See the README on Github for more info][3]

>Breaking changes
>
>The NuGet packages have changed their package naming in the move from v2.x.x to >v3.0.0
>
>Rx-Main is now System.Reactive
>Rx-Core is now System.Reactive.Core
>Rx-Interfaces is now System.Reactive.Interfaces
>Rx-Linq is now System.Reactive.Linq
>Rx-PlatformServices is now System.Reactive.PlatformServices
>Rx-Testing is now Microsoft.Reactive.Testing


  [1]: https://www.nuget.org/packages/System.Reactive/
  [2]: https://dotnet.myget.org/gallery/rx
  [3]: https://github.com/Reactive-Extensions/Rx.NET/blob/2029682095663ec2d3b214af0ad1fd83963fb060/README.md

## Throttling a stream


## Get a running aggregation
Suppose you have a hot observable for which you would love to keep the count of. It could be the `IObservable<StockTick>` and you want to keep count of the average trade volume. You can use `Scan` for that.


    var tradeVolume = stockTicks.Select(e => e.Price)
        .Scan(0.0m, (aggregated, newtick) => aggregated + newtick)
        .Select((aggregated, index) => aggregated / (index + 1))

Now you can simply subscribe to your trade volume which is live updated upon receipt of every new Tick.

    var subscription = tradeVolume.Subscribe(vol => Console.WriteLine("New trade volume is {0}", vol);

