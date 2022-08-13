---
title: "Getting started with google-analytics"
slug: "getting-started-with-google-analytics"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting on board with Google Analytics
 1. **Getting a GA Account**:
     If you don’t have an Analytics account, create one. If you do have an Analytics account, sign in. Both options are available at google.com/analytics
 
 2. **Setting up a property in your Analytics account:**
     A property represents your website or app where the data gets aggregated.

 3. **Create a view:** 
      Views let you create filtered perspectives of your data. When you create a property one view is created by default. You can create multiple views based on the requirement and filter the reports based on the reporting structure.
 
 4. **Embed the Analytics in your website**: Go to property > tracking info and get the tracking code which looks like below


    <!-- Google Analytics -->
    <script>
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
    
    ga('create', 'UA-XXXXX-Y', 'auto');
    ga('send', 'pageview');
    </script>
    <!-- End Google Analytics -->

 5. The string '**UA-XXXXX-Y**' should be replaced with the property ID (also called the "tracking ID") of the Google Analytics property you wish to track.

With these simple steps, your website will be ready to send the pageviews to GA. 





## Adding analytics.js to your website
Add the following code (known as the "JavaScript tracking snippet") to your site's templates.

The code should be added before the closing </head> tag, and the string **'UA-XXXXX-Y'** should be replaced with the [property ID][1] (also called the "tracking ID") of the Google Analytics property you wish to track.

    <!-- Google Analytics -->
    <script>
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
    
    ga('create', 'UA-XXXXX-Y', 'auto');
    ga('send', 'pageview');
    </script>
    <!-- End Google Analytics -->

The above code does four main things:

1. Creates a `<script>` element that starts asynchronously downloading the analytics.js JavaScript library from https://www.google-analytics.com/analytics.js
2. Initializes a global ga function (called the `ga()` command queue) that allows you to schedule commands to be run once the analytics.js library is loaded and ready to go.
3. Adds a command to the `ga()` command queue to create a new tracker object for the property specified via the `'UA-XXXXX-Y'` parameter.
4. Adds another command to the `ga()` command queue to send a pageview to Google Analytics for the current page.

  [1]: https://support.google.com/analytics/answer/1032385

## Overview
Google Analytics is used to track user activity on your website or mobile application. 

To set up google-analytics on a website you will need to get a snippet of JavaScript code from Google that you embed in the head of each page on your site that you want to track user activity.

Get the code snippet at [www.google.com/analytics](https://www.google.com/analytics), and on the Admin tab select “Create new account” from the dropdown menu of the account column on the left.

## Track pages called by AJAX and non-html content
To track so called "virtual pageviews", use the `ga('send')` method right after your asynchronous request:

**Syntax:**  `ga('send', 'pageview', 'path to your virtual page');`

**Example (Simple Link):**

    <a href="http://example.com/my.pdf"
       onClick="ga('send', 'pageview', '/virtual/my.pdf');">Download PDF</a> 

**Example (JQuery AJAX):**

    $.ajax({
        url: '/ajax-url/file.json',
        data: {page: 4},
        success: function(data) {
            ga('send', 'pageview', '/ajax-url/file.json');
            console.log("Got response",data); 
        },
        dataType: 'json',
        method: 'GET'
    });


----------


Sources:

 - http://stackoverflow.com/questions/24199037/how-do-i-get-google-analytics-to-track-pages-called-by-ajax/24199142#24199142
 - [Tracking virtual pageviews - developers.google.com][1]


  [1]: https://developers.google.com/analytics/devguides/collection/analyticsjs/single-page-applications#tracking_virtual_pageviews

## Alternative async tracking snippet
While the JavaScript tracking snippet described above ensures the script will be loaded and executed asynchronously on all browsers, it has the disadvantage of not allowing modern browsers to preload the script.

The alternative async tracking snippet below adds support for preloading, which will provide a small performance boost on modern browsers, but can degrade to synchronous loading and execution on IE 9 and older mobile browsers that do not recognize the async script attribute. Only use this tracking snippet if your visitors primarily use modern browsers to access your site.

        <!-- Google Analytics -->
    <script>
    window.ga=window.ga||function(){(ga.q=ga.q||[]).push(arguments)};ga.l=+new Date;
    ga('create', 'UA-XXXXX-Y', 'auto');
    ga('send', 'pageview');
    </script>
    <script async src='https://www.google-analytics.com/analytics.js'></script>
    <!-- End Google Analytics -->

## Using Plugins
Plugins are scripts that enhance the functionality of analytics.js to aid in measuring user interaction. Plugins are typically specific to a set of features that may not be required by all Google Analytics users, such as ecommerce or cross-domain tracking, and are therefore not included in analytics.js by default.

This guide explains how to require and use analytics.js plugins.

The require command takes the name of a plugin and registers it for use with the `ga()` command queue. If the plugin accepts configuration options, those options can be passed as the final argument to the require command.

The following is the full require command's signature:

    ga('[trackerName.]require', pluginName, [pluginOptions]);

For example, here is how you would require the Enhanced Ecommerce plugin for use with the default tracker:

    ga('require', 'ec');

And here is how you would require the Display Features plugin for a tracker named "myTracker" and pass a configuration option that overrides the default cookie name value:

    ga('myTracker.require', 'displayfeatures', {
      cookieName: 'display_features_cookie'
    });

## What data does the tracking snippet capture?
When you add either of these tracking snippets to your website, you send a pageview for each page your users visit. Google Analytics processes this data and can infer a great deal of information including:

The total time a user spends on your site.
The time a user spends on each page and in what order those pages were visited.
What internal links were clicked (based on the URL of the next pageview).
In addition, the IP address, user agent string, and initial page inspection analytics.js does when creating a new tracker is used to determine things like the following:

The geographic location of the user.
What browser and operating system are being used.
Screen size and whether Flash or Java is installed.
The referring site.

