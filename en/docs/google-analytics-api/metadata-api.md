---
title: "Metadata api"
slug: "metadata-api"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
  - HTTP GET https://www.googleapis.com/analytics/v3/metadata/{reportType}/columns?key={APIKey}
  - HTTP GET https://www.googleapis.com/analytics/v3/metadata/{reportType}/columns?access_token={Access_token}





## Parameters
| Parameter name | Description |
| ------ | ------ |
| reportType | Report type. Allowed Values: **ga**. Where **ga** corresponds to the [Core Reporting API][1].  |


  [1]: https://developers.google.com/analytics/devguides/reporting/core/v3/

The [Metadata API][1] returns the list and attributes of columns (i.e. dimensions and metrics) exposed in the [Google Analytics reporting APIs][2] (v2,v3 and v4). Attributes returned include UI name, description, segments support, and more.

You can use the Metadata API to:

 - Automatically discover new columns
 - Access all dimensions and metrics attributes for Google Analytics reporting APIs

This is the same list as is in the [Dimensions & Metrics Explorer][3]. 

> Note: Real-time and Multi-Channel Funnels dimensions and metrics are
> not currently available.


  [1]: https://developers.google.com/analytics/devguides/reporting/metadata/v3/
  [2]: https://developers.google.com/analytics/devguides/reporting/core/v3/
  [3]: https://developers.google.com/analytics/devguides/reporting/core/dimsmets

## Rest Example
Calls to the [Metadata API][1] are with HTTP Get:

**Using Public API Key**

>GET https://www.googleapis.com/analytics/v3/metadata/ga/columns?key={YOUR_API_KEY}

**Using Access token from either Oauth2 or Service account authentication**

>GET https://www.googleapis.com/analytics/v3/metadata/ga/columns?access_token={Authentcated_Access_Token}


  [1]: https://developers.google.com/analytics/devguides/reporting/metadata/v3/

## Java example
uses the [Java Client library][1]


    /**
     * 1. Execute a Metadata Request
     * An application can request columns data by calling the list method on the Analytics service object.
     * The method requires an reportType parameter that specifies the column data to retrieve.
     * For example, the following code requests columns for the ga report type.
     */
    
    try {
      Columns results = getMetadata(analytics);
      // Success
    
    } catch (GoogleJsonResponseException e) {
      // Catch API specific errors.
      handleApiError(e);
    } catch (IOException e) {
      // Catch general parsing network errors.
      e.printStackTrace();
    }
    
    /**
     * 2. Print out the Columns data
     * The components of the result can be printed out as follows:
     */
    
    private static Columns getMetadata(Analytics analytics) throws IOException {
      String reportType = "ga";
      return analytics.metadata()
          .columns()
          .list(reportType)
          .execute();
    }
    
    
    private static void printMetadataReport(Columns results) {
      System.out.println("Metadata Response Report");
      printReportInfo(results);
      printAttributes(results.getAttributeNames());
      printColumns(results.getItems());
    }
    
    
    private static void printReportInfo(Columns results) {
      System.out.println("## Metadata Report Info ##");
      System.out.println("Kind: " + results.getKind());
      System.out.println("Etag: " + results.getEtag());
      System.out.println("Total Results: " + results.getTotalResults());
      System.out.println();
    }
    
    
    private static void printAttributes(List<String> attributeNames) {
      System.out.println("## Attribute Names ##");
      for (String attribute : attributeNames) {
        System.out.println(attribute);
      }
    }
    
    
    private static void printColumns(List<Column> columns) {
      System.out.println("## Columns ##");
    
      for (Column column : columns) {
        System.out.println();
        System.out.println("Column ID: " + column.getId());
        System.out.println("Kind: " + column.getKind());
    
        Map<String, String> columnAttributes = column.getAttributes();
    
        for (Map.Entry<String, String> attribute: columnAttributes.entrySet()) {
          System.out.println(attribute.getKey() + ": " + attribute.getValue());
        }
      }
    }

Note: first version copied from [Metadata.list][2]


  [1]: https://developers.google.com/api-client-library/java/
  [2]: https://developers.google.com/analytics/devguides/reporting/metadata/v3/reference/metadata/columns/list

## PHP Example
Uses the [PHP client library][1]


    /**
     * 1. Execute a Metadata Request
     * An application can request columns data by calling the list method on the Analytics service object.
     * The method requires an reportType parameter that specifies the column data to retrieve.
     * For example, the following code requests columns for the ga report type.
     */
    
    try {
    
      $results = $analytics->metadata_columns->listMetadataColumns('ga');
      // Success
    
    } catch (apiServiceException $e) {
      // Handle API service exceptions.
      $error = $e->getMessage();
    }
    
    
    /**
     * 2. Print out the Columns data
     * The components of the result can be printed out as follows:
     */
    
    function printMetadataReport($results) {
      print '<h1>Metadata Report</h1>';
      printReportInfo($results);
      printAttributes($results);
      printColumns($results);
    }
    
    
    function printReportInfo(&$results) {
      $html = '<h2>Report Info</h2>';
      $html .= <<<HTML
    <pre>
    Kind                  = {$results->getKind()}
    Etag                  = {$results->getEtag()}
    Total Results         = {$results->getTotalResults()}
    </pre>
    HTML;
      print $html;
    }
    
    
    function printAttributes(&$results) {
      $html = '<h2>Attribute Names</h2><ul>';
      $attributes = $results->getAttributeNames();
      foreach ($attributes as $attribute) {
        $html .= '<li>'. $attribute . '</li>';
      }
      $html .= '</ul>';
      print $html;
    }
    
    
    function printColumns(&$results) {
      $columns = $results->getItems();
      if (count($columns) > 0) {
        $html = '<h2>Columns</h2>';
        foreach ($columns as $column) {
          $html .= '<h3>' . $column->getId() . '</h3>';
          $column_attributes = $column->getAttributes();
          foreach ($column_attributes as $name=>$value) {
            $html .= <<<HTML
    <pre>
    {$name}: {$value}
    </pre>
    HTML;
          }
        }
      } else {
        $html = '<p>No Results Found.</p>';
      }
      print $html;
    }

Note: original version copied from [metadata.list][2]


  [1]: https://github.com/google/google-api-php-client
  [2]: https://developers.google.com/analytics/devguides/reporting/metadata/v3/reference/metadata/columns/list

## Python Example
Uses the [Python client library][1]


    # 1. Execute a Metadata Request
    # An application can request columns data by calling the list method on the Analytics service object.
    # The method requires an reportType parameter that specifies the column data to retrieve.
    # For example, the following code requests columns for the ga report type.
    
    try:
      results = service.metadata().columns().list(reportType='ga').execute()
    
    except TypeError, error:
      # Handle errors in constructing a query.
      print ('There was an error in constructing your query : %s' % error)
    
    except HttpError, error:
      # Handle API errors.
      print ('Arg, there was an API error : %s : %s' %
             (error.resp.status, error._get_reason()))
    
    # 2. Print out the Columns data
    # The components of the result can be printed out as follows:
    
    def print_metadata_report(results):
      print 'Metadata Response Report'
      print_report_info(results)
      print_attributes(results.get('attributeNames'))
      print_columns(results)
    
    
    def print_report_info(columns):
      print "Metadata Report Info"
      if columns:
        print 'Kind           = %s' % columns.get('kind')
        print 'Etag           = %s' % columns.get('etag')
        print 'Total Results  = %s' % columns.get('totalResults')
    
    
    def print_attributes(attributes):
      if attributes:
        print 'Attribute Names:'
        for attribute in attributes:
          print attribute
    
    def print_columns(columns_data):
      if columns_data:
        print 'Columns:'
    
        columns = columns_data.get('items', [])
    
        for column in columns:
          print
          print '%15s = %35s' % ('Column ID', column.get('id'))
          print '%15s = %35s' % ('Kind', column.get('kind'))
    
          column_attributes = column.get('attributes', [])
    
          for name, value in column_attributes.iteritems():
            print '%15s = %35s' % (name, value)

Note: original version copied from [metadata.list][2]


  [1]: https://developers.google.com/api-client-library/python/
  [2]: https://developers.google.com/analytics/devguides/reporting/metadata/v3/reference/metadata/columns/list

## C# example
Uses the [.Net Client library][1]

>PM>  Install-Package Google.Apis.Analytics.v3 


    var metadataService = new AnalyticsMetaDataService(new BaseClientService.Initializer()
                {
                    ApiKey = {Public API KEY},
                    ApplicationName = "Metadata api",
                });
    
    var result = Service.Metadata.Columns.List("ga").Execute();


  [1]: https://github.com/google/google-api-dotnet-client

