---
title: "Logging JavaScript errors within Google Analytics"
slug: "logging-javascript-errors-within-google-analytics"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The example above has two tracking events, [Event Tracking][1] and [Exception Tracking][2].

**Event Tracking** will allow you to see JS errors in real-time. Under `Real Time -> Events` sections. 

Unfortunately, your error messages will be limited by 500 Bytes, so you will not be able to understand a problem properly, however you will know that something is going wrong.


**Exception Tracking** will give you more detailed report, with full error message and browser information. 

You can generate Exception Tracking report with [Custom Reports][3].


  [1]: https://developers.google.com/analytics/devguides/collection/analyticsjs/events
  [2]: https://developers.google.com/analytics/devguides/collection/analyticsjs/exceptions
  [3]: https://support.google.com/analytics/answer/1151300

## Following code will submit all JavaScript errors into Google Analytics
    <script>
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
        (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
      m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','//www.google-analytics.com/analytics.js','_watchdog');
    
    _watchdog('create', 'UA-xxxxxxx-x', 'auto');
    
    window.onerror = function(msg) {
      _watchdog('send', 'exception', { 'exDescription': msg });

      _watchdog('send', 'event', {
        eventCategory: 'javascript',
        eventAction: 'error',
        eventLabel: msg,
        transport: 'beacon'
      });
    }
    </script>

