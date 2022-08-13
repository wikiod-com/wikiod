---
title: "Getting started with google-drive-sdk"
slug: "getting-started-with-google-drive-sdk"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
In order to access the Google drive API or Google drive SDK you must first register your application on [Google Developer console][1] and enable the google drive API. 

After that its a good idea to use one of the official google client libraries to access the API.

| Documentation|     Samples |
| ------ | ------ |
| [Google API Client Library for Java](https://developers.google.com/api-client-library/java/) | [Java samples](https://developers.google.com/api-client-library/java/apis/)
| [Google API Client Library for JavaScript (beta)](https://developers.google.com/api-client-library/javascript/start/start-js) | [JavaScript samples](https://developers.google.com/api-client-library/javascript/samples/samples)
| [Google API Client Library for .NET](https://developers.google.com/api-client-library/dotnet/get_started) | [.NET samples](https://developers.google.com/api-client-library/dotnet/apis/)
| [Google API Client Library for Objective-C](https://github.com/google/google-api-objectivec-client) | [Objective-C samples](https://github.com/google/google-api-objectivec-client)
| [Google API Client Library for PHP (beta)](https://developers.google.com/api-client-library/php/) | [PHP samples](https://github.com/google/google-api-php-client/tree/master/examples)
| [Google API Client Library for Python](https://developers.google.com/api-client-library/python/) | [Python samples](https://github.com/google/google-api-python-client/tree/master/samples)

There are a number of other client libraries in early-stage of development a full list can be found at [Google APIs Client libraries][2]


  [1]: https://console.developers.google.com
  [2]: http://Google%20APIs%20Client%20Libraries

## List Files
As most of the information contained within Google drive is private user data.  You must have an access token in order to access the information.   Access tokens can be retrieved via the [Oauth2 authentication process][1].

    GET https://www.googleapis.com/drive/v2/files?access_token={Valid Access Token}

Response

    {
      "kind": "drive#fileList",
      "etag": etag,
      "selfLink": string,
      "nextPageToken": string,
      "nextLink": string,
      "items": [
        [files Resource][2]
      ]
    }


  [1]: https://developers.google.com/drive/v2/web/about-auth
  [2]: https://developers.google.com/drive/v2/reference/files#resource

