---
title: "Getting started with kendo-ui"
slug: "getting-started-with-kendo-ui"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
To prepare your web site or web application to use Kendo UI, simply add the style and script files in the `<head>` section of your page. 

CDN services are provided for the minified versions of **official** Kendo UI releases, for both HTTP and HTTPS protocols.

JQuery is also required to use Kendo UI and it is also provided by CDN.

<!-- language-all: lang-html -->

    <!-- The common stylesheet for basic styling -->
    <link rel="stylesheet" href="http://kendo.cdn.telerik.com/[version number]/styles/kendo.common.min.css" />
    <!-- The theme stylesheet for the specific theme -->
    <link rel="stylesheet" href="http://kendo.cdn.telerik.com/[version number/styles/kendo.[theme name].min.css" />

    <!-- Note that the version number here is that of Kendo UI, and not that of jQuery -->
    <script src="http://kendo.cdn.telerik.com/[version number]/js/jquery.min.js"></script>
    <script src="http://kendo.cdn.telerik.com/[version number]/js/kendo.all.min.js"></script>

In the snippet above, the kendo.all script is used, which includes all basic kendo widgets (the DataViz suite of widgets is not included). 

You can alternatively pick and choose which widgets or components you want by loading the script file for that component. For example, change "all" to "calendar" to get the script for the calendar widget:
    <script src="http://kendo.cdn.telerik.com/[version number]/js/kendo.calendar.min.js"></script>

For a list of all files you can use, see Kendo UI's [Only What You Need][1] page

Here is a functional snippet for style and script tags that you can use to quickly get started:

    <link rel="stylesheet" href="http://kendo.cdn.telerik.com/2016.2.714/styles/kendo.common.min.css" />
    <link rel="stylesheet" href="http://kendo.cdn.telerik.com/2016.2.714/styles/kendo.blueopal.min.css" />

    <script src="http://kendo.cdn.telerik.com/2016.2.714/js/jquery.min.js"></script>
    <script src="http://kendo.cdn.telerik.com/2016.2.714/js/kendo.all.min.js"></script>
  [1]: http://docs.telerik.com/kendo-ui/intro/installation/what-you-need

