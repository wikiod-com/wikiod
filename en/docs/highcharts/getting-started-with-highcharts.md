---
title: "Getting started with highcharts"
slug: "getting-started-with-highcharts"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Ways to get Highcharts:

 - Load directly from [Highcharts CDN][1]
 - [Download][2] files from official website.
 - Through npm;


    npm install highcharts

 - Through bower;


    bower install highcharts


----------


To load the main library from vendor's CDN, simply add the following to your code:

    <script src="https://code.highcharts.com/highcharts.js"></script>

Supplementary libraries, such as the exporting module should be loaded **after** your `highcharts.js` declaration.

Calling the libraries directly from Highcharts will provide you the most up-to-date version. However, if you have specific charts that function best with a certain version or you wish to optimize your website's performance, you may consider storing the files locally.

The following resources offer detailed information on installing and configuring Highcharts, as well as supplementary libraries and modules you can use to customize your charts beyond the default installation.

 - [Installation instructions for Highcharts][3]
 - [Export module overview][4]
 - [Highcharts file service, including all modules and versions][1]


  [1]: https://code.highcharts.com/
  [2]: http://www.highcharts.com/download
  [3]: http://www.highcharts.com/docs/getting-started/installation
  [4]: http://www.highcharts.com/docs/export-module/export-module-overview

## Hello World
Begin by including `highcharts.js` in your `index.html`

    <html>
      <head>
        <script src="http://code.highcharts.com/highcharts.js"></script>
      </head>

Add a `<div>` to contain your chart

      <body>
        <div id="chart">
          <!-- Chart goes here -->
        </div>

Specify the configuration to create the chart. The mininum configuration required to create a chart is - 
 - Where does the chart go? - [chart.renderTo][1]
 - What is the data to be plotted? - There are [a few ways][2] to feed in the data to be plotted; the most common among them being the [series][3] object.

        var chartOptions = {
            chart: {
                renderTo: 'chart'  
            },
            series: [{      
                data: [1, 2]
            }]  
        };
        var chartHandle = Highcharts.Chart(chartOptions);
           
This creates a plot as follows - [fiddle][4].

There are numerous configuration options that can be added to the chart, a few common ones being,

 - [chart.type][5] - What type of chart do you want to plot?
 - [title][6] - Configuration for the title of the chart
 - [xAxis/yAxis][7] - Configuration for the x-axis/y-axis of the chart

A complete list of all configuration options can be found [here][8].

        <script>
        var chartOptions = {
          chart: {
            renderTo: 'chart',
            type: 'bar'
          },
          title: {
            text: 'Hello Highcharts'
          },
          xAxis: {
            categories: ['Hello', 'World']
          },
          yAxis: {
            title: 'Value'
          },
          series: [{
            name: 'Highcharts Intro',
            data: [1, 2]
          }]
        };
    
        var chart = new Highcharts.Chart(chartOptions);
        
        </script>
      </body>
    </html>

[JSFiddle Example][9]

A good place to start in the Highcharts doc would be [here][10].


  [1]: http://api.highcharts.com/highcharts/chart.renderTo
  [2]: http://www.highcharts.com/docs/working-with-data/data-intro
  [3]: http://www.highcharts.com/docs/chart-concepts/series
  [4]: https://jsfiddle.net/bbd0m74f/
  [5]: http://www.highcharts.com/docs/chart-and-series-types/chart-types
  [6]: http://www.highcharts.com/docs/chart-concepts/title-and-subtitle
  [7]: http://www.highcharts.com/docs/chart-concepts/axes
  [8]: http://api.highcharts.com/highcharts/
  [9]: https://jsfiddle.net/q2cn88wo/9/
  [10]: http://www.highcharts.com/docs/chart-concepts/understanding-highcharts

## Colors
In [Highcharts](http://www.highcharts.com), there is an array containing the default colors for the chart's series. When all colors are used, new colors are pulled from the start again. 

Defaults colors for **version 4.x and 5.x** are:

    colors: [
        '#7cb5ec', 
        '#434348', 
        '#90ed7d', 
        '#f7a35c', 
        '#8085e9', 
        '#f15c80', 
        '#e4d354', 
        '#2b908f', 
        '#f45b5b', 
        '#91e8e1'
    ]


In **Highcharts 3.x**, the default colors were:

    colors: [
        '#2f7ed8', 
        '#0d233a', 
        '#8bbc21', 
        '#910000', 
        '#1aadce', 
        '#492970', 
        '#f28f43', 
        '#77a1e5', 
        '#c42525', 
        '#a6c96a'
]

In **Highcharts 2.x**, the default colors were:

    colors: [
        '#4572A7', 
        '#AA4643', 
        '#89A54E', 
        '#80699B', 
        '#3D96AE', 
        '#DB843D', 
        '#92A8CD', 
        '#A47D7C', 
        '#B5CA92'
    ]

## Compatibility
 | Brand | Versions Supported|
 | ------ | ------ |
 | Internet Explorer| 6.0 + |
 | Firefox | 2.0 + |
 | Chrome | 1.0 + |
 | Safari | 4.0 + |
 | Opera | 9.0 + |
 | iOS (Safari) | 3.0 + |
 | Android Browser | 2.0 + |
 
 > [Highcharts](http://www.highcharts.com/) supports [jQuery](https://jquery.com/) version **1.6+** for legacy browsers, and **2.0+** for modern browsers.

