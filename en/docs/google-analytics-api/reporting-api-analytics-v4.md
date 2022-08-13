---
title: "Reporting API (Analytics v4)"
slug: "reporting-api-analytics-v4"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

The [Google Analytics Reporting API V4][1] is the most advanced programmatic method to access report data in [Google Analytics][2]. With the Google Analytics Reporting API, you can:

 - Build custom dashboards to display [Google Analytics][2] data.
 - Automate complex reporting tasks to save time.
 - Integrate your [Google Analytics][2] data with other business applications.

**Features**

[Google Analytics][2] is built upon a powerful data reporting infrastructure. The Google Analytics Reporting API V4 gives you access to the power of the [Google Analytics][2] platform. The API provides these key features:

 - Metric expressions: The API allows you to request not only built-in metrics but also combination of metrics expressed in mathematical operations. For example, you can use the expression ga:goal1completions/ga:sessions to request the goal completions per number of sessions.
 - Multiple date ranges: The API allows you in a single request to get data in two date ranges.
 - Cohorts and Lifetime value: The API has a rich vocabulary to request Cohort and Lifetime value reports.
 - Multiple segments: The API enables you to get multiple segments in a single request.


  [1]: https://developers.google.com/analytics/devguides/reporting/core/v4/
  [2]: https://analytics.google.com/analytics/web

## Single report Example using Oauth2 C#
This example uses the official Google .net Client library.

> PM>  [Install-Package Google.Apis.AnalyticsReporting.v4][1]

**Authorization**
Requires one of the following OAuth scopes:

> - https://www.googleapis.com/auth/analytics.readonly
> - https://www.googleapis.com/auth/analytics


**Oauth2**

    // These are the scopes of permissions you need.
    string[] scopes = new string[] { AnalyticsReportingService.Scope.AnalyticsReadonly };   // View your Google Analytics Data

    UserCredential credential;
    using (var stream = new FileStream(clientSecretJson, FileMode.Open, FileAccess.Read))
    {
        string credPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
        credPath = Path.Combine(credPath, ".credentials/", System.Reflection.Assembly.GetExecutingAssembly().GetName().Name);

        // Requesting Authentication or loading previously stored authentication for userName
       credential = GoogleWebAuthorizationBroker.AuthorizeAsync(GoogleClientSecrets.Load(stream).Secrets,
           scopes,
          userName,
          CancellationToken.None,
          new FileDataStore(credPath, true)).Result;
    }

    // Create Reporting API service.
    var service = new AnalyticsReportingService(new BaseClientService.Initializer()
                {
                 HttpClientInitializer = credential,
                 ApplicationName = string.Format("{0} Authentication", System.Reflection.Assembly.GetExecutingAssembly().GetName().Name),
                });

**Reporting Request**

    // Create the DateRange object.
    DateRange June2015 = new DateRange() { StartDate = "2015-01-01", EndDate = "2015-06-30" };
    DateRange June2016 = new DateRange() { StartDate = "2016-01-01", EndDate = "2016-06-30" };
    List<DateRange> dateRanges = new List<DateRange>() { June2016, June2015 };           
                           
    
    // Create the ReportRequest object.
    // This should have a large number of rows
    ReportRequest reportRequest = new ReportRequest
    {
        ViewId = ConfigurationManager.AppSettings["GoogleAnaltyicsViewId"],
        DateRanges = dateRanges,
        Dimensions = new List<Dimension>() { new Dimension() { Name = "ga:date" }, new Dimension() { Name = "ga:usertype" } },
        Metrics = new List<Metric>() { new Metric() { Expression= "ga:users" }, new Metric() { Expression = "ga:sessions" } },
        PageSize = 1000,
    };
                
    List<ReportRequest> requests = new List<ReportRequest>();
    requests.Add(reportRequest);
    
    
    var getReport = new GetReportsRequest() { ReportRequests = requests };
    var response = service.Reports.BatchGet(getReport).Execute();


  [1]: https://www.nuget.org/packages/Google.Apis.AnalyticsReporting.v4/

## Single Report Example Rest
API requests are HTTP POST with the access token attached at the end of the API end point.

> Authorization Requires one of the following OAuth scopes:
> 
> • https://www.googleapis.com/auth/analytics.readonly<br>
> • https://www.googleapis.com/auth/analytics

Note when posting the data use `ContentType = "application/Json";`


    https://analyticsreporting.googleapis.com/v4/reports:batchGet?Access_token={from auth}
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



