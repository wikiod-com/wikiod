---
title: "Getting started with chart.js"
slug: "getting-started-with-chartjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
[Chart.js](http://www.chartjs.org) can be included in several different ways:

**NPM**

Run the following command on your NPM project directory

    npm install chart.js --save

**CDN**

Include a script tag in your HTML linking to the chart.js CDN

    <html>
       <body>    
          <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.1.6/Chart.bundle.min.js"></script>
       </body>
    </html>

Latest version can be found at [cdnjs.com/libraries/Chart.js](https://cdnjs.com/libraries/Chart.js).

**Local Copy**

A local copy can also be hosted on your server. You can get the lasted version from their [GitHub](https://github.com/chartjs/Chart.js/releases).

    <html>
       <body>    
          <script type="text/javascript" src="/Path/To/Chart.bundle.min.js"></script>
       </body>
    </html>

For more information about getting chart.js installed see [www.chartjs.org/docs/](http://www.chartjs.org/docs/#getting-started-installation).

## Minimal Chart Example
Depending on the version of Chart.JS you are using (the current one being 2.X), the syntax is different to create a minimal example of a bar chart ([JSFiddle Demo for 2.X](https://jsfiddle.net/ywdguuz7/2/)).

<h1>Chart.js 2.X</h1>

<!-- language: lang-html -->

    <html>
        <body>
            <canvas id="myChart" width="400" height="400"></canvas>
            <script>
                  var ctx = document.getElementById("myChart");
                  var myChart = new Chart(ctx, {
                      type: 'bar',
                      data: {
                          labels: ["Group 1", "Group 2", "Group 3"],
                          datasets: [{
                              label: 'Groups',
                              data: [12, 19, 3]
                          }]
                      }
                  });
            </script>
        </body>
    </html>

A slightly more advanced version of this can be found in the [chart.js documentation](http://www.chartjs.org/docs/#getting-started-creating-a-chart) ([JSFiddle Demo](https://jsfiddle.net/ywdguuz7/1/)).
<hr>

# Chart.js 1.X
However, if you need to use the legacy version, first take a look at the [documentation on Github](https://github.com/chartjs/Chart.js/tree/v1.1.1/docs).

Now here is a minimal example of a bar chart ([JSFiddle Demo](https://jsfiddle.net/cocw07xx/)) :

<!-- language: lang-html -->

    <html>
        <body>
            <canvas id="myChart" width="400" height="400"></canvas>
            <script>
                var ctx = document.getElementById("myChart");
                var myChart= new Chart(ctx).Bar({
                    labels: ["Group 1", "Group 2", "Group 3"],
                    datasets: [
                    {
                        label: "Group",
                        data: [12, 19, 3]
                    }]
                });
            </script>
        </body>
    </html>

A slightly more advanced version of this can be found in the [Github documentation](https://github.com/chartjs/Chart.js/blob/v1.1.1/docs/02-Bar-Chart.md) ([JSFiddle Demo](https://jsfiddle.net/cocw07xx/1/)).

