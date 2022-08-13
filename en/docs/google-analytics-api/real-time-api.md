---
title: "Real-time API"
slug: "real-time-api"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
 - GET https://www.googleapis.com/analytics/v3/data/realtime?ids=[Analytics view Id]&metrics=[Real-time metrics]&access_token=[Access_token_from_Authentcation]
 - GET https://www.googleapis.com/analytics/v3/data/realtime?ids=[Analytics view Id]&metrics=[Real-time metrics]&dimensions=[Real-time metrics]&access_token=[Access_token_from_Authentcation]

## Parameters


| (Required) Parameter name| Description|
| ------ | ------ |
| ids | Unique table ID for retrieving Analytics data. Table ID is of the form ga:XXXX, where XXXX is the Analytics view (profile) ID. |
| metrics | A comma-separated list of Analytics metrics. E.g., 'rt:activeUsers'. At least one metric must be specified. |
| **(Optional) Parameter name**| **Description**|
| dimensions | A comma-separated list of real-time dimensions. E.g., 'rt:medium,rt:city'. |
| filters | A comma-separated list of dimension or metric filters to be applied to real-time data. |
| max-results | The maximum number of entries to include in this feed. |
| sort | A comma-separated list of dimensions or metrics that determine the sort order for real-time data. |
| **(Standard) Parameter name**| **Description**|
| callback| Name of the JavaScript callback function that handles the response. Used in JavaScript JSON-P requests. |
| prettyPrint | Returns the response in a human-readable format if true. Default value: true. When this is false, it can reduce the response payload size, which might lead to better performance in some environments. |
| quotaUser | Lets you enforce per-user quotas from a server-side application even in cases when the user's IP address is unknown. This can occur, for example, with applications that run cron jobs on App Engine on a user's behalf. You can choose any arbitrary string that uniquely identifies a user, but it is limited to 40 characters.Overrides userIp if both are provided.  Learn more about capping usage.|
| userIp | Lets you enforce per-user quotas when calling the API from a server-side application. Learn more about capping usage. |



> **The Real Time Reporting API, in limited beta, is available for**
> **developer preview only. [Sign up][1] to access the API.**

The [Real Time Reporting API][2] enables you to request real time data—for example, real time activity on your property—for an authenticated user.

You can use the Real Time Reporting API to:

 - Display active viewers of a page and create a sense of urgency for users looking at an item with finite inventory. 
 -  Display the most popular content such as the top 10 active pages.
 -  Create and display a real time dashboard.

**Authorization**

Calls to the Google Analytics Real-time API requires authorization with at least one of the following scopes (read more about authentication and authorization).

| Scope| access granted |
| ------ | ------ |
| https://www.googleapis.com/auth/analytics | Full access to Google Analytics accounts up to the level of the authenticated users access. |
| https://www.googleapis.com/auth/analytics.readonly| Read only access to the Authenticated users Google Analytics accounts|







  [1]: https://docs.google.com/forms/d/e/1FAIpQLSc9OpoDGB3tBD7oy1OG9fyum8KBIxs-2ihPCsHp13WTnM-SSQ/viewform
  [2]: https://developers.google.com/analytics/devguides/reporting/realtime/v3/

## PHP Example
Uses the [PHP client library][1]

    /**
     * 1.Create and Execute a Real Time Report
     * An application can request real-time data by calling the get method on the Analytics service object.
     * The method requires an ids parameter which specifies from which view (profile) to retrieve data.
     * For example, the following code requests real-time data for view (profile) ID 56789.
     */
    $optParams = array(
        'dimensions' => 'rt:medium');
    
    try {
      $results = $analytics->data_realtime->get(
          'ga:56789',
          'rt:activeUsers',
          $optParams);
      // Success. 
    } catch (apiServiceException $e) {
      // Handle API service exceptions.
      $error = $e->getMessage();
    }
    
    
    /**
     * 2. Print out the Real-Time Data
     * The components of the report can be printed out as follows:
     */
    
    function printRealtimeReport($results) {
      printReportInfo($results);
      printQueryInfo($results);
      printProfileInfo($results);
      printColumnHeaders($results);
      printDataTable($results);
      printTotalsForAllResults($results);
    }
    
    function printDataTable(&$results) {
      if (count($results->getRows()) > 0) {
        $table .= '<table>';
    
        // Print headers.
        $table .= '<tr>';
    
        foreach ($results->getColumnHeaders() as $header) {
          $table .= '<th>' . $header->name . '</th>';
        }
        $table .= '</tr>';
    
        // Print table rows.
        foreach ($results->getRows() as $row) {
          $table .= '<tr>';
            foreach ($row as $cell) {
              $table .= '<td>'
                     . htmlspecialchars($cell, ENT_NOQUOTES)
                     . '</td>';
            }
          $table .= '</tr>';
        }
        $table .= '</table>';
    
      } else {
        $table .= '<p>No Results Found.</p>';
      }
      print $table;
    }
    
    function printColumnHeaders(&$results) {
      $html = '';
      $headers = $results->getColumnHeaders();
    
      foreach ($headers as $header) {
        $html .= <<<HTML
    <pre>
    Column Name       = {$header->getName()}
    Column Type       = {$header->getColumnType()}
    Column Data Type  = {$header->getDataType()}
    </pre>
    HTML;
      }
      print $html;
    }
    
    function printQueryInfo(&$results) {
      $query = $results->getQuery();
      $html = <<<HTML
    <pre>
    Ids         = {$query->getIds()}
    Metrics     = {$query->getMetrics()}
    Dimensions  = {$query->getDimensions()}
    Sort        = {$query->getSort()}
    Filters     = {$query->getFilters()}
    Max Results = {$query->getMax_results()}
    </pre>
    HTML;
    
      print $html;
    }
    
    function printProfileInfo(&$results) {
      $profileInfo = $results->getProfileInfo();
    
      $html = <<<HTML
    <pre>
    Account ID               = {$profileInfo->getAccountId()}
    Web Property ID          = {$profileInfo->getWebPropertyId()}
    Internal Web Property ID = {$profileInfo->getInternalWebPropertyId()}
    Profile ID               = {$profileInfo->getProfileId()}
    Profile Name             = {$profileInfo->getProfileName()}
    Table ID                 = {$profileInfo->getTableId()}
    </pre>
    HTML;
    
      print $html;
    }
    
    function printReportInfo(&$results) {
      $html = <<<HTML
      <pre>
    Kind                  = {$results->getKind()}
    ID                    = {$results->getId()}
    Self Link             = {$results->getSelfLink()}
    Total Results         = {$results->getTotalResults()}
    </pre>
    HTML;
    
      print $html;
    }
    
    function printTotalsForAllResults(&$results) {
      $totals = $results->getTotalsForAllResults();
    
      foreach ($totals as $metricName => $metricTotal) {
        $html .= "Metric Name  = $metricName\n";
        $html .= "Metric Total = $metricTotal";
      }
    
      print $html;
    }
Original version coped from official [documentation][2]


  [1]: https://github.com/google/google-api-php-client
  [2]: https://developers.google.com/analytics/devguides/reporting/realtime/v3/reference/data/realtime/get

