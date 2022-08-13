---
title: "Getting started with ibm-watson-cognitive"
slug: "getting-started-with-ibm-watson-cognitive"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting API credentials
To authenticate to Watson services, you need credentials for each service that you plan to use. Depending on the service, you will need to pass a username and password with Basic Authentication, or you will need to pass an API key in a parameter for each request you make.

How to get credentials for a Watson service:
 1. [Sign up for Bluemix][1] and log in.
 2. Go to the service page for your desired Watson service:
     - [AlchemyLanguage and AlchemyData News][2]
     - [Conversation][3]
     - [Dialog][4]
     - [Document Conversion][5]
     - [Language Translation][6]
     - [Natural Language Classifier][7]
     - [Personality Insights][8]
     - [Retrieve and Rank][9]
     - [Speech to Text][10]
     - [Text to Speech][11]
     - [Tone Analyzer][12]
     - [Tradeoff Analytics][13]
     - [Visual Recognition][14]
 3.  Select your desired plan, and click CREATE:

[![Bluemix service catalog page][15]][15]

 4. Click the "Service Credentials" button from your service dashboard page to view your credentials. If you aren't taken to the service dashboard automatically, go to your [Bluemix dashboard][16] and click on your desired service instance.

[![Location of credentials on your Bluemix service dashboard][17]][17]



 


  [1]: https://console.ng.bluemix.net/registration/
  [2]: https://console.ng.bluemix.net/catalog/services/alchemyapi
  [3]: https://console.ng.bluemix.net/catalog/services/conversation/
  [4]: https://console.ng.bluemix.net/catalog/services/dialog/
  [5]: https://console.ng.bluemix.net/catalog/services/document-conversion/
  [6]: https://console.ng.bluemix.net/catalog/services/language-translation/
  [7]: https://console.ng.bluemix.net/catalog/services/natural-language-classifier/
  [8]: https://console.ng.bluemix.net/catalog/services/personality-insights/
  [9]: https://console.ng.bluemix.net/catalog/services/retrieve-and-rank/
  [10]: https://console.ng.bluemix.net/catalog/services/speech-to-text/
  [11]: https://console.ng.bluemix.net/catalog/services/text-to-speech/
  [12]: https://console.ng.bluemix.net/catalog/services/tone-analyzer/
  [13]: https://console.ng.bluemix.net/catalog/services/tradeoff-analytics/
  [14]: https://console.ng.bluemix.net/catalog/services/visual-recognition/
  [15]: http://i.stack.imgur.com/iGDkW.jpg
  [16]: https://console.ng.bluemix.net/dashboard/
  [17]: http://i.stack.imgur.com/bNJtl.jpg
  [18]: https://github.com/watson-developer-cloud/android-sdk
  [19]: https://github.com/watson-developer-cloud/iOS-sdk
  [20]: https://github.com/watson-developer-cloud/java-sdk
  [21]: https://github.com/watson-developer-cloud/node-sdk
  [22]: https://github.com/watson-developer-cloud/python-sdk
  [23]: https://github.com/watson-developer-cloud/unity-sdk

## Using Watson Developer Cloud SDKs
The quickest way to get started with Watson services is to use the Watson Developer Cloud SDKs. The following GitHub repositories contain installation instructions and basic usage examples:

 - [Android][1]
 - [iOS][2]
 - [Java][3]
 - [Node.js][4]
 - [Python][5]
 - [Unity][6]

For example, here's how to make an AlchemyLanguage API call with the Node.js SDK:

Install the SDK:

    $ npm install watson-developer-cloud

Save the following code to a file (we'll call it *app.js*). Make sure you replace `API_KEY` with your API key.

<!-- language: lang-js -->

```
// Instantiate the service 
var AlchemyLanguageV1= require('watson-developer-cloud/alchemy-language/v1');
var alchemy_language = AlchemyLanguageV1({
  api_key: 'API_KEY'
})

var parameters = {
  extract: [
    'entities',
    'keywords'
  ]
  url: 'https://www.ibm.com/us-en/'
};

alchemy_language.combined(parameters, function (err, response) {
  if (err)
    console.log('error:', err);
  else
    console.log(JSON.stringify(response, null, 2));
});
```
Run the app:

    $ node app.js 

  [1]: https://github.com/watson-developer-cloud/android-sdk
  [2]: https://github.com/watson-developer-cloud/ios-sdk
  [3]: https://github.com/watson-developer-cloud/java-sdk
  [4]: https://github.com/watson-developer-cloud/node-sdk
  [5]: https://github.com/watson-developer-cloud/python-sdk
  [6]: https://github.com/watson-developer-cloud/unity-sdk

## Calling Watson APIs with curl

Depending on the service, you will either need to use Basic Authentication with a `username` and `password` or pass an `apikey` as a parameter in each request. 


Some services also support [token authentication][1].



GET using Tone Analyzer:

<!-- language: lang-bash -->
    curl -X GET \
    -u "username":"password" \
    -d "version=2016-05-19" \
    -d "text=Hey! Welcome to Watson Tone Analyzer!" \
    "https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone

POST using AlchemyLanguage:

<!-- language: lang-bash -->

    curl -X POST \
    -d "apikey=YOUR_API_KEY" \
    -d "url=www.ibm.com" \
    "https://gateway-a.watsonplatform.net/calls/url/URLGetRankedKeywords"



  [1]: https://www.ibm.com/watson/developercloud/doc/getting_started/gs-tokens.shtml#tokens

