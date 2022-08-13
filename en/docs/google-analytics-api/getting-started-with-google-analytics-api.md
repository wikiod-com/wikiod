---
title: "Getting started with google-analytics-api"
slug: "getting-started-with-google-analytics-api"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
The [Google Analytics APIs][1] allow you to access data within Google Analytics.  It should not be confused with the [measurement protocol][2] which is used for inserting data into [Google Analytics][3].

The Google Analytics API is split into serval parts.


**Google Analytics Reporting APIs**


1. The [Google Analytics Core Reporting API v3][4] gives you access to most of the report data in Google Analytics. With the Core Reporting API you can:

     • Build custom dashboards to display Google Analytics data.<br>
     • Save time by automating complex reporting tasks.<br>
     • Integrate your Google Analytics data with other business applications.<br>

2. The [Google Analytics Reporting API V4][5] is the most advanced programmatic method to access report data in Google Analytics. With the Google Analytics Reporting API, you can:

     • Build custom dashboards to display Google Analytics data.<br>
     • Automate complex reporting tasks to save time.<br>
     • Integrate your Google Analytics data with other business applications.<br>


2. The [Real Time Reporting API][6] enables you to request real time data—for example, real time activity on a view—for an authenticated user.
You can use the Real Time Reporting API to:

     • Display active viewers of a page and create a sense of urgency for users looking at an item with finite inventory. <br>
     • Display the most popular content such as the top 10 active pages.<br>
     • Create and display a real time dashboard.<br>

> The Real Time Reporting API, in limited beta, is available for
> developer preview only. [Sign up][7] to access the API.

3. The [Multi-Channel Funnels Reporting API][8] enables you to request Multi-Channel Funnels data for an authenticated user. Data is derived from conversion path data, which shows user interactions with various traffic sources over multiple sessions prior to converting. This allows you to analyze how multiple marketing channels influence conversions over time. For more details on what data is available, read the  About Multi-Channel Funnels, as well as  About Multi-Channel Funnels Data. With the Multi-Channel Funnels Reporting API you can:

     • Create custom reports using Multi-Channel Funnels data. For example, you could use the  Top Conversion Paths data to report on attributes such as relative position of interactions in a conversion path.<br>
     • Integrate Multi-Channel Funnels data with your business data. For example, you could correlate online conversion data with offline sales data and media cost data to arrive at a more complete picture of marketing ROI.<br>
     • Display Multi-Channel Funnels in new environments. For instance, you could create visualizations and other presentations of the data that communicate the value of different marketing channels in driving conversions.<br>

**Helpers**

1. The [Metadata API][9] returns the list and attributes of columns (i.e. dimensions and metrics) exposed in the Google Analytics reporting APIs. Attributes returned include UI name, description, segments support, and more. You can use the Metadata API to:

     • Automatically discover new columns.<br>
     • Access all dimensions and metrics attributes for Google Analytics reporting APIs.<br>

> Note: This only returns metadata for the Core Reporting API and the
> Reporting API.  Not real-time metadata.

2. The [Google Analytics Embed API][10] is a JavaScript library that allows you to easily create and embed a dashboard on a 3rd party website in a matter of minutes. It gives you a set of pluggable components that can work together to build complex tools, making it both simple and powerful at the same time.



**Configuration APIs**

1. The [Analytics Management API][11] allows for programmatic access to the Google Analytics configuration data. You can build applications to more efficiently manage large or complex Analytics accounts. Large companies with many properties can automate account setup. Even if you are building a reporting application the Management API provides you tools to navigate your account. You can use the Google Analytics Management API to: 

     • List all the Account, Property and View information for a user.<br>
     • Manage Properties, Views, and Goals.<br>
     • Manage user permissions for an account hierarchy.<br>
     • Retrieve a View ID to use with the Core Reporting API.<br>
     • Determine which goals are active and access their configured names.<br>
     • Manage Links between Analytics properties and AdWords accounts. <br>
     • Manage  Remarketing Audiences.<br> 

> Write operations in the Management API (e.g. create, update, delete,
> patch) for Web Property, View (Profile), and Goal resources is
> currently available as a developer preview in **limited beta**. If
> you're interested in using these features,  [request access to the
> beta][12].

2. The [Provisioning API][13] can be used to create new Google Analytics accounts and enable Google Analytics for your customers at scale. It is intended for qualified service providers and large partners. For example, you could use the Provisioning API as part of a new user onboarding process to create a new Google Analytics account for a client and then use additional Management API resources to programmatically configure the account and link it to AdWords. This can all be automated and initiated from within your own admin or reporting interface.

> **The Provisioning API is available by invitation only. We are not
> currently accepting new projects.**


  [1]: https://developers.google.com/analytics/devguides/reporting/
  [2]: https://developers.google.com/analytics/devguides/collection/protocol/v1/?hl=en
  [3]: https://analytics.google.com/analytics/web/
  [4]: https://developers.google.com/analytics/devguides/reporting/core/v3/
  [5]: https://developers.google.com/analytics/devguides/reporting/core/v4/
  [6]: https://developers.google.com/analytics/devguides/reporting/realtime/v3/
  [7]: https://docs.google.com/forms/d/e/1FAIpQLSc9OpoDGB3tBD7oy1OG9fyum8KBIxs-2ihPCsHp13WTnM-SSQ/viewform
  [8]: https://developers.google.com/analytics/devguides/reporting/mcf/v3/
  [9]: https://developers.google.com/analytics/devguides/reporting/metadata/v3/
  [10]: https://developers.google.com/analytics/devguides/reporting/embed/v1/
  [11]: https://developers.google.com/analytics/devguides/config/mgmt/v3/
  [12]: https://docs.google.com/forms/d/e/1FAIpQLSf01NWo9R-SOHLKDUH0U4gWHNDBIY-gEI-zqBMG1Hyh3_hHZw/viewform
  [13]: https://developers.google.com/analytics/devguides/config/provisioning/v3/

## Hello World Reporting API - Rest
    POST https://analyticsreporting.googleapis.com/v4/reports:batchGet?access_token={Access token from auth request}
    {
      "reportRequests":[
      {
        "viewId":"XXXX",
        "dateRanges":[
          {
            "startDate":"2015-06-15",
            "endDate":"2015-06-30"
          }],
        "metrics":[
          {
            "expression":"ga:sessions"
          }],
        "dimensions": [
          {
            "name":"ga:browser"
          }]
        }]
    }



## Accessing Google Analytics APIs
You can technically access the Google Analytics APIs using any programing language that can handle a [HTTP Post][1] or [HTTP Get][2] request.  

That being said, Google has also created a number of official standard client libraries to help you with this. Using a standard client library for your chosen programming language can be much easier than coding it from the ground up yourself.

**OFFICAL Client libraries with Google Analytics API support:**

1. Google APIs PHP Client library - [GitHub][3]
2. Google APIs .Net Client library - [GitHub][4] [NuGet][5]
3. Google APIs Python Client library - [GitHub][6]
4. Google APIs Java Client library  - [link][7]
5. Google APIs  Objective-C library  - [GitHub][8]

There are more libraries [here][9].


  [1]: https://en.wikipedia.org/wiki/POST_(HTTP)
  [2]: https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods
  [3]: https://github.com/google/google-api-php-client
  [4]: https://github.com/google/google-api-dotnet-client
  [5]: https://www.nuget.org/profiles/google-apis-packages
  [6]: https://github.com/google/google-api-python-client
  [7]: https://developers.google.com/api-client-library/java/
  [8]: https://github.com/google/google-api-objectivec-client
  [9]: http://Google%20APIs%20Client%20Libraries

