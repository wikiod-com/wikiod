---
title: "Getting started with dygraphs"
slug: "getting-started-with-dygraphs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Create a file called `hello.html` with the following content:

    <!doctype html>
    <html>
    <head>
        <script src="//cdnjs.cloudflare.com/ajax/libs/dygraph/2.0.0/dygraph.js"></script>
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/dygraph/2.0.0/dygraph.css">
    </head>
    <body>
        <div id="chart-div"></div>
        <script type="text/javascript">
            g = new Dygraph(
                document.getElementById('chart-div'),
                [
                  [0, 0, 0],
                  [1, 1, 1],
                  [2, 2, 4],
                  [3, 3, 9]
                ], {
                  labels: ['X', 'Apples', 'Oranges']
                });
        </script>
    </body>
    </html>

This loads the dygraphs JavaScript and CSS from a [CDN][].

The bit in the `<script>` tag creates the chart. It specifies a data set with two series: "Apples" and "Oranges", as well as the x-axis.

For more, see the [dygraphs tutorial][1]

[cdn]: https://en.wikipedia.org/wiki/Content_delivery_network
[1]: http://dygraphs.com/tutorial.html

