---
title: "Integration of Google's BigQuery with web application"
slug: "integration-of-googles-bigquery-with-web-application"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Integration of Google's BigQuery API with web application
In this tutorial , I will explain how to integrate Google's BigQuery API with web application. My web application is going to get the data using BigQuery and plot a graph using d3.js and Javascript.

Each project on Google Developers Console has a clientID and you need to copy the clientID and put it as a config:

    var gconfig = {
        'client_id': 'ClientID',
        'scope': 'https://www.googleapis.com/auth/bigquery'
    };

BigQuery API can be accessed in the following way:

    $.getScript("https://apis.google.com/js/client.js", function(d) {
      function loadGAPI() {
        setTimeout(function() {
          if (!gapi.client) {
            loadGAPI();
          } else {
            loadBigQuery();
          }
        }, 500);
      }
      
      function loadBigQuery() {
        gapi.client.load('bigquery', 'v2');
        setTimeout(function() {
          if (!gapi.client.bigquery) {
            loadBigQuery();
          } else {
            onClientLoadHandler();
          }
        }, 500);
      }
      
      loadGAPI();
    });

Also you'll need to mention the query which you are going to retrieve the data:

    function runQuery() {
        var request = gapi.client.bigquery.jobs.query({
          'projectId': "bigdatameetup-83116",
          'timeoutMs': '30000',
          'query': 'SELECT DATE(date ) as date,SUM(INTEGER(orders)) as total_orders FROM [bigdatameetup-83116:Demo_Backup.orders] GROUP BY date ORDER BY date LIMIT 1000; '
        });
        request.execute(function(response) {
          var bqData = [];
    
          response.result.rows.forEach(function(d) {
            bqData.push({"date": d3.time.format("%Y-%m-%d").parse(d.f[0].v),
              "total_orders": +d.f[1].v});
          });
          
          drawLineChart(bqData);
        });
      }

The rest is the visualization, i.e the creation of Line Chart using d3.js:

    function drawLineChart(bqData) {
      var WIDTH = config.width, HEIGHT = config.height;
      var Y_AXIS_LABEL = "total_orders";
      var X_DATA_PARSE = d3.time.format("%d-%b-%y").parse;
      var Y_DATA_PARSE = vida.number;
      var X_DATA_TICK = d3.time.format("%b-%y");
      var X_AXIS_COLUMN = "date";
      var Y_AXIS_COLUMN = "total_orders";
      var margin = {top: 20, right: 20, bottom: 30, left: 50},
          width = WIDTH - margin.left - margin.right,
          height = HEIGHT - margin.top - margin.bottom;
      var x = d3.time.scale()
          .range([0, width]);
        var y = d3.scale.linear()
          .range([height, 0]);
      var xAxis = d3.svg.axis()
          .scale(x)
          .orient("bottom")
          .tickFormat(X_DATA_TICK);
      var yAxis = d3.svg.axis()
          .scale(y)
          .orient("left")
          .tickFormat(function(d) {
            return d / 1000000 + "M";
          });
      var line = d3.svg.line()
          .interpolate("basis")
          .x(function(d) { return x(d.x_axis); })
          .y(function(d) { return y(d.y_axis); });
      
      var svg = d3.select("#canvas-svg").append("svg")
          .attr("width", width + margin.left + margin.right)
          .attr("height", height + margin.top + margin.bottom)
        .append("g")
          .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
      
      bqData.forEach(function(d) {
        d.x_axis = d[X_AXIS_COLUMN];
        d.y_axis = d[Y_AXIS_COLUMN];
      });
      
      bqData.sort(function(a, b) {
        return (new Date(a.x_axis)) - (new Date(b.x_axis));
      });
      
      x.domain(d3.extent(bqData, function(d) { return d.x_axis; }));
      y.domain(d3.extent(bqData, function(d) { return d.y_axis; }));
      
      svg.append("g")
          .attr("class", "x axis")
          .attr("transform", "translate(0," + height + ")")
          .call(xAxis);
      
      svg.append("g")
          .attr("class", "y axis")
          .call(yAxis)
        .append("text")
          .attr("transform", "rotate(-90)")
          .attr("y", 6)
          .attr("dy", ".71em")
          .style("text-anchor", "end")
          .text(Y_AXIS_LABEL);
      
      svg.append("path")
          .datum(bqData)
          .attr("class", "line")
          .attr("d", line);
      
      }

In this example, I have chosen 'Amount' as x-axis and 'Date' as y axis from the public dataset:

> nyc_taxi_public

You can find the full working sample in this link. 

[`BigQuery Integration with WebApplication`][1]


  [1]: https://vida.io/documents/FXxufAWNPWuaPW5v2

