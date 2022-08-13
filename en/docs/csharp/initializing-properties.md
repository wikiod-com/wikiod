---
title: "Initializing Properties"
slug: "initializing-properties"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

When deciding on how to create a property, start with an auto-implemented property for simplicity and brevity. 

Switch to a property with a backing field only when circumstances dictate. If you need other manipulations beyond a simple set and get, you may need to introduce a backing field.

## C# 6.0: Initialize an Auto-Implemented Property
Create a property with getter and/or setter and initialize all in one line:

    public string Foobar { get; set; } = "xyz";

## Initializing Property with a Backing Field
    public string Foobar { 
        get { return _foobar; }
        set { _foobar = value; }
    }
    private string _foobar = "xyz";

## Initializing Property in Constructor
    class Example
    {
        public string Foobar { get; set; }
        public List<string> Names { get; set; }
        public Example()
        {
            Foobar = "xyz";
            Names = new List<string>(){"carrot","fox","ball"};
        }
    }

## Property Initialization during object instantiation
Properties can be set when an object is instantiated.

    var redCar = new Car 
    {
        Wheels = 2,
        Year = 2016,
        Color = Color.Red
    };

