---
title: "Themes"
slug: "themes"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Setting up a Theme
First we need to reference the Styles we want to use.

    <link rel="stylesheet" href="[file path]/kendo.common.min.css" />
    <link rel="stylesheet" href="[file path]/kendo.[Theme].min.css" />

Now most of the controls use the `[Theme]`, given in the above Style Resource.

Some controls like *Chart, TreeMap, Diagram, StockChart, Sparkline, RadialGauge, and LinearGauge* are rendered with JavaScript computed Layouts and need some extra configuration at initialization.

For example the Chart:

    $("#chart").kendoChart({
        theme: "blueOpal",
        //...
    });
List of themes:

    "black", "blueOpal", "bootstrap", "default", "flat", "highContrast", "material", "materialBlack", "metro", "metroBlack", "moonlight", "silver", "uniform", "fiori"(kendo ui pro), "nova"

To set them globally you are able to override Kendo this way:

    //The Themable Elements
    var themable = ["Chart", "TreeMap", "Diagram", "StockChart", "Sparkline", "RadialGauge", "LinearGauge"];
    
    //Check if kendo.dataviz is available
    if (kendo.dataviz) {
      
      //Set for each control the default theme
      for (var i = 0; i < themable.length; i++) {
        var widget = kendo.dataviz.ui[themable[i]];
    
        if (widget) {
          widget.fn.options.theme = "blueOpal";
        }
      }
    }

