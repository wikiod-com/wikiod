---
title: "MKDistanceFormatter"
slug: "mkdistanceformatter"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## String from distance
Given a `CLLocationDistance` (simply a `Double` representing meters), output a user-readable string:

    let distance = CLLocationDistance(42)
    let formatter = MKDistanceFormatter()
    let answer = formatter.stringFromDistance(distance)
    // answer = "150 feet"

**Objective-C**

    CLLocationDistance distance=42;
    MKDistanceFormatter *formatter=[[MKDistanceFormatter alloc]init];
    NSString *answer=[formatter stringFromDistance:distance];
    // answer = "150 feet"

By default, this respects the user's locale.

## Distance units
`import Mapkit` Set `units` to one of `.Default, .Metric, .Imperial, .ImperialWithYards`:

    formatter.units = .Metric
    var answer = formatter.stringFromDistance(distance)
    // "40 m"

    formatter.units = .ImperialWithYards
    answer = formatter.stringFromDistance(distance)
    // "50 yards"

**Objective-C**

    MKDistanceFormatter *formatter=[[MKDistanceFormatter alloc]init];
    formatter.units=MKDistanceFormatterUnitsMetric;
    NSString *answer=[formatter stringFromDistance:distance];
    //40 m
   
    formatter.units=MKDistanceFormatterUnitsImperialWithYards;
    NSString *answer=[formatter stringFromDistance:distance];
    //50 yards

 

## Unit style
Set `unitStyle` to one of `.Default, .Abbreviated, .Full`:

    formatter.unitStyle = .Full
    var answer = formatter.stringFromDistance(distance)
    // "150 feet"
    
    formatter.unitStyle = .Abbreviated
    answer = formatter.stringFromDistance(distance)
    // "150 ft"

**Objective-C**

    formatter.unitStyle=MKDistanceFormatterUnitStyleFull;
    NSString *answer=[formatter stringFromDistance:distance];
    // "150 feet"
    
     formatter.unitStyle=MKDistanceFormatterUnitStyleAbbreviated;
     NSString *answer=[formatter stringFromDistance:distance];
    // "150 ft"

