---
title: "Getting started with google-visualization"
slug: "getting-started-with-google-visualization"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Loading and Running
Google currently has two ways to load the JS library for Google Visualization (a.k.a Google Charts), **gstatic loader** (`https://www.gstatic.com/charts/loader.js`) and 
**jsapi** (`https://www.google.com/jsapi`).

>The gstatic loader is recommended because Google is transitioning away from jsapi to the gstatic loader.
[See transition reference][1]

In either case, you must first include one of the loaders with a `script` tag, typically in the `head` of your document, like this:

    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>

Once you have included the loader in your webpage, you can use it to load the desired library packages by calling a `load` function.

**For Loader.js**

    google.charts.load('current', {packages: ['corechart']});

**For JSAPI**

    google.load('visualization', '1', {'packages':['corechart']});

But after you load the library packages, you must wait for them to finish being loaded before proceeding to use them.  The way to wait is to set up a callback by calling a `setOnLoadCallback` function.

Sample Code (for the gstatic loader):

    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>
    <script>
     google.charts.load('current', {packages: ['corechart']});
     google.charts.setOnLoadCallback(drawChart);
          function drawChart() {
    
            var data = new google.visualization.DataTable();
            data.addColumn('string', 'Group');
            data.addColumn('number', 'Gender');
            data.addRows([
              ['Males', 10],
              ['Females', 5]
            ]);
    
            var options = {
                'title':'Gender distribution',
                'width':300,
                'height':300};
            var chart = new google.visualization.PieChart(
                document.getElementById('gender_chart'));
            chart.draw(data, options);
          }
        </script>

HTML:

    <div id="gender_chart"></div>

[**JSFIDDLE**][2]


  [1]: https://developers.google.com/chart/interactive/docs/basic_load_libs#frozen-versions
  [2]: https://jsfiddle.net/_pirateX/ue88hus0/

