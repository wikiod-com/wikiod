---
title: "Getting started with amcharts"
slug: "getting-started-with-amcharts"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Intro
Before you can use amCharts, you will need to include required JavaScript Libraries.

There is one main library that is required for all amCharts operations - **amcharts.js**. It needs to be included first and is mandatory.

Each chart type requires chart-type specific include. For example Serial chart, will also require **serial.js**, a Pie chart will need **pie.js**, etc.

If on the same web page you will be displaying several different chart types, you will need to include all chart-type-specific includes that are being displayed on that page.

## Creating a chart
The chart consists out of HTML container and the JavaScript code that instantiates a chart in it.

HTML
----
We use a `<div>` element as chart container.

    <div id="chartdiv" style="height: 300px;"></div>

JavaScript
----------
To instantiate the chart we use `AmCharts.makeChart()` function.
The first parameter is an id of the container to place chart in, the second an object with chart config.

At the very least it must contain `type` parameter, which holds chart type.

    var chart = AmCharts.makeChart("chartdiv", {
      "type": "serial",
      "theme": "light",
      "dataProvider": [{
        "country": "USA",
        "visits": 2025
      }, {
        "country": "China",
        "visits": 1882
      }, {
        "country": "Japan",
        "visits": 1809
      }, {
        "country": "Germany",
        "visits": 1322
      }, {
        "country": "UK",
        "visits": 1122
      }, {
        "country": "France",
        "visits": 1114
      }, {
        "country": "India",
        "visits": 984
      }],
      "graphs": [{
        "fillAlphas": 0.9,
        "lineAlpha": 0.2,
        "type": "column",
        "valueField": "visits"
      }],
      "categoryField": "country"
    });

## Loading from amCharts CDN
AmCharts provides a load-balanced, reliable CDN for loading the libraries directly from our web server. Use **https://www.amcharts.com/lib/3/** as a base URL for includes.

    <script src="https://www.amcharts.com/lib/3/amcharts.js"></script>
    <script src="https://www.amcharts.com/lib/3/serial.js"></script>

## Loading libraries from your own server
To do that, [download][1] a required amCharts product ZIP archive.

Unzip it and place somewhere on your web server. I.e. in **/amcharts/** sub-directory.

Then simply load them using `<script>` tags:

    <script src="amcharts/amcharts.js"></script>
    <script src="amcharts/serial.js"></script>

## Additional includes
Besides main required functional includes, you may need other includes, like themes and plugins. Those reside in **/themes/** and **/plugins/** sub-directories respectively.

I.e.:

    <script src="https://www.amcharts.com/lib/3/amcharts.js"></script>
    <script src="https://www.amcharts.com/lib/3/serial.js"></script>
    <script src="https://www.amcharts.com/lib/3/themes/light.js"></script>
    <script src="https://www.amcharts.com/lib/3/plugins/dataloader/dataloader.min.js"></script>

  [1]: https://www.amcharts.com/download/

