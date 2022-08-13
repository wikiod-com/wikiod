---
title: "Error Responses"
slug: "error-responses"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
 - 400 invalidParameter Indicates that a request parameter has an invalid value. 
 - 400 badRequest Indicates that the query was invalid. E.g., parent ID was missing or the combination of dimensions or metrics requested was not valid. 

 - 403 insufficientPermissions Indicates that the user does not have sufficient permissions for the entity specified in the query. 

 - 403 dailyLimitExceeded Indicates that user has exceeded the daily quota (either per project or per view (profile)).

 - 403 usageLimits.userRateLimitExceededUnreg Indicates that the application needs to be registered in the Google API Console. 

 - 403 userRateLimitExceeded Indicates that the user rate limit has been exceeded. The maximum rate limit is 10 qps per IP address. 

 - 403 rateLimitExceeded Indicates that the global or overall project rate limits have been exceeded. Retry using exponential back-off. You need to slow down the rate at which you are sending the requests. 

 - 403 quotaExceeded Indicates that the 10 concurrent requests per view (profile) in the Core Reporting API has been reached. 

 - 429 RESOURCE_EXHAUSTED AnalyticsDefaultGroupCLIENT_PROJECT-1d Indicates that the Requests per day per project quota has been exhausted. 
  
 - 429 RESOURCE_EXHAUSTED AnalyticsDefaultGroupCLIENT_PROJECT-100s Indicates that the Requests per 100 seconds per project quota has been exhausted.  

 - 429 RESOURCE_EXHAUSTED AnalyticsDefaultGroupUSER-100s Indicates that the requests per 100 seconds per user per project quota has been exhausted.  

 - 429 RESOURCE_EXHAUSTED DiscoveryGroupCLIENT_PROJECT-100s Indicates that the discovery requests per 100 seconds quota has been exhausted.  

 - 500 internalServerError Unexpected internal server error occurred. Do not retry this query more than once. 

 - 503 backendError Server returned an error. Do not retry this query more than once. 


## Parameters
| Name | Description |
| ------ | ------ |
| domain | Location of the error ex: global |
| reason| Reason for the error   |
| message | A message explaining the error and a possible solution. |
| locationType | The location type of the error ex: paramater   |
| location| The actual location of the error   |



If an request is successful, the API returns a 200 HTTP status code along with the requested data in the body of the response.

If an error occurs with a request, the API returns an HTTP status code and reason in the response based on the type of error. Additionally, the body of the response contains a detailed description of what caused the error. 


## 400 invalidParameter
It is important to read the error response that is returned by the Google Analytics API server.  In a lot of cases they can tell you exactly what is wrong.    



    400 invalidParameter
    
    {
     "error": {
      "errors": [
       {
        "domain": "global",
        "reason": "invalidParameter",
        "message": "Invalid value '-1' for max-results. Value must be within the range: [1, 1000]",
        "locationType": "parameter",
        "location": "max-results"
       }
      ],
      "code": 400,
      "message": "Invalid value '-1' for max-results. Value must be within the range: [1, 1000]"
     }
    }

In this case the message:

>"message": "Invalid value '-1' for max-results. Value must be within the range: [1, 1000]",

Is telling us that we sent a -1 value for the parameter max-results which is not valid we can only send a value from 1-1000.

