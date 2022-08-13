---
title: "Plugins"
slug: "plugins"
draft: false
images: []
weight: 9400
type: docs
toc: true
---

Starting with v2.1.0, you can create plugins for chart.js! [Plugin official documentation](http://www.chartjs.org/docs/#advanced-usage-creating-plugins)

## Plugins Introduction
Plugins are a way for a developer to modify a chart as it is being created. Chart.js calls all plugins at the following chart states:

 - Start of initialization
 - End of initialization
 - Start of update
 - After the chart scales have calculated
 - Start of datasets update
 - End of datasets update
 - End of update (before render occurs)
 - Start of draw
 - End of draw
 - Before datasets draw
 - After datasets draw
 - Resize
 - Before an animation is started

**Creating a plugin**

To create a plugin, create a JavaScript object with appropriate named functions for any chart state you wish to modify (listed above). After you have your plugin object, pass it to `Chart.pluginService.register(PLUGIN_OBJECT_NAME);` to let Chart.js know to register the plugin.

**Minimal Plugin Example**

    // Create the plugin object with functions for all the chart states
    var simplePlugin = {
      beforeInit: function(chartInstance) {},
      afterInit: function(chartInstance) {},
    
      resize: function(chartInstance, newChartSize) {},
    
      beforeUpdate: function(chartInstance) {},
      afterScaleUpdate: function(chartInstance) {},
      beforeDatasetsUpdate: function(chartInstance) {},
      afterDatasetsUpdate: function(chartInstance) {},
      afterUpdate: function(chartInstance) {},
    
      // This is called at the start of a render. It is only called once, even if the animation will run for a number of frames. Use beforeDraw or afterDraw
      // to do something on each animation frame
      beforeRender: function(chartInstance) {},
    
      // Easing is for animation
      beforeDraw: function(chartInstance, easing) {},
      afterDraw: function(chartInstance, easing) {},

      // Before the datasets are drawn but after scales are drawn
      beforeDatasetsDraw: function(chartInstance, easing) {},
      afterDatasetsDraw: function(chartInstance, easing) {},
    
      destroy: function(chartInstance) {}
    };
    
    // Let Chart.js know about the new plugin
    Chart.pluginService.register(simplePlugin);

Currently this minimal plugin does not do anything. To make this plugin useful one would need to add code to the functions that modifies the chart.


## Draw Horizonal Lines
Create horizontal lines with a label. This could be used to show notable values in the chart data (e.g. min, max, average). [JSFiddle Demo](http://jsfiddle.net/Lgtzcr7v/2/)


    var horizonalLinePlugin = {
      afterDraw: function(chartInstance) {
        var yScale = chartInstance.scales["y-axis-0"];
        var canvas = chartInstance.chart;
        var ctx = canvas.ctx;
        var index;
        var line;
        var style;
    
        if (chartInstance.options.horizontalLine) {
          for (index = 0; index < chartInstance.options.horizontalLine.length; index++) {
            line = chartInstance.options.horizontalLine[index];
    
            if (!line.style) {
              style = "rgba(169,169,169, .6)";
            } else {
              style = line.style;
            }
    
            if (line.y) {
              yValue = yScale.getPixelForValue(line.y);
            } else {
              yValue = 0;
            }
    
            ctx.lineWidth = 3;
    
            if (yValue) {
              ctx.beginPath();
              ctx.moveTo(0, yValue);
              ctx.lineTo(canvas.width, yValue);
              ctx.strokeStyle = style;
              ctx.stroke();
            }
    
            if (line.text) {
              ctx.fillStyle = style;
              ctx.fillText(line.text, 0, yValue + ctx.lineWidth);
            }
          }
          return;
        };
      }
    };
    Chart.pluginService.register(horizonalLinePlugin);

[Credit to L Bahr for the example](http://stackoverflow.com/a/38339579/6194193)

