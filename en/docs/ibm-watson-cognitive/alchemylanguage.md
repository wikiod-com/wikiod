---
title: "AlchemyLanguage"
slug: "alchemylanguage"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

AlchemyLanguage is a collection of text analysis methods that provide deeper insight into your text or HTML content. See the [Getting Started][1] topic to learn how to get started with AlchemyLanguage and other Watson services. For more AlchemyLanguage details and examples, see the [API reference][2] and [documentation][3].

Size limits
===========

 - HTML content before text cleaning: **600 KB**
 - Source text, after text cleaning: **50 KB**
 - Calls that use Custom Models: **5 KB**

Language support
=======
To see which languages are supported for each function, refer to each function's entry in the [API reference][4].

Language detection
=======
By default, AlchemyLanguage automatically detects the language of your source text. You can manually specify the language of your content with the `language` query parameter. (e.g. `language=spanish`)

Text cleaning
=============

When you use an HTML or URL function of the API, AlchemyLanguage cleans the content to prepare the source text for the analysis. The `sourceText` parameter allows you to customize the cleaning process with the following options:

 - `cleaned_or_raw` (default) -- Removes website elements such as links, ads, etc. If cleaning fails, raw web page text is used
 - `cleaned`-- Removes website elements such as links, ads, etc.
 - `raw` -- Uses raw web page text with no cleaning
 - `cquery` -- Uses the visual constraints query that you specify in the `cquery` parameter. See the [documentation][5] for details about visual constraints queries.
 - `xpath` -- Uses the XPath query that you specify in the `xpath` parameter
 - `xpath_or_raw` -- Uses the results of an XPath query, falling back to plain text if the XPath query returns nothing
 - `cleaned_and_xpath` -- Uses the results of an XPath query on cleaned web page text
 


  [1]: https://www.wikiod.com/ibm-watson-cognitive/getting-started-with-ibm-watson-cognitive#Getting API credentials
  [2]: http://www.ibm.com/watson/developercloud/alchemy-language/api/v1/
  [3]: https://www.ibm.com/watson/developercloud/doc/alchemylanguage/
  [4]: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/alchemy-language/api/v1/
  [5]: http://www.ibm.com/watson/developercloud/doc/alchemylanguage/visual_constraints.shtml

## Combined Call: use multiple functions in a single API call (Node.js)
The Combined Call method allows you to use multiple AlchemyLanguage functions in one request. This example uses a Combined Call to get entities and keywords from the IBM website and returns sentiment information for each result.

> This example requires [AlchemyLanguage service credentials][1] and [Node.js][2].  

1. Use a command-line interface to install the [Watson Developer Cloud Node.js SDK][3]:


    $ npm install watson-developer-cloud
 
2. Save the following code to an *app.js* file in the same directory. Make sure you replace `API_KEY` with your AlchemyAPI key:



<!-- language: lang-js -->

```
var AlchemyLanguageV1 = require('watson-developer-cloud/alchemy-language/v1');
var alchemy_language = AlchemyLanguageV1({
  api_key: 'API_KEY'
})

var parameters = {
  extract: 'entities,keywords',
  sentiment: 1, 
  url: 'https://www.ibm.com/us-en/'
};

alchemy_language.combined(parameters, function (err, response) {
  if (err)
    console.log('error:', err);
  else
    console.log(JSON.stringify(response, null, 2));
});
``` 
3. Run the app:


    $ node app.js


  [1]: https://www.wikiod.com/ibm-watson-cognitive/getting-started-with-ibm-watson-cognitive#Getting API credentials#t=201609142342226653189
  [2]: https://nodejs.org/
  [3]: https://github.com/watson-developer-cloud/node-sdk

## Sentiment Analysis: get sentiment information for specific phrases in text (Node.js)
AlchemyLanguage's Targeted Sentiment feature can search your content for target phrases and return sentiment information for each result.

> This example requires [AlchemyLanguage service credentials][1] and [Node.js][2]

1. Use a command-line interface to install the [Watson Developer Cloud Node.js SDK][3]:


    $ npm install watson-developer-cloud


2. Save the following code to an *app.js* file in the same directory. Make sure you replace `API_KEY` with your AlchemyAPI key:

<!-- language: lang-js -->

```
var AlchemyLanguageV1 = require('watson-developer-cloud/alchemy-language/v1');
var alchemy_language = new AlchemyLanguageV1({
  api_key: 'API_KEY'
})

var parameters = {
  text: 'Grapes are the best! I hate peaches.',
  targets: [
    'grapes',
    'peaches'
  ]
};

alchemy_language.sentiment(parameters, function (err, response) {
  if (err)
    console.log('error:', err);
  else
    console.log(JSON.stringify(response, null, 2));
});
```

3. Run the app:


    $ node app.js

  [1]: https://www.wikiod.com/ibm-watson-cognitive/getting-started-with-ibm-watson-cognitive#Getting API credentials#t=201609142342226653189
  [2]: https://nodejs.org/
  [3]: https://github.com/watson-developer-cloud/node-sdk/

## Concepts: identify concepts from a webpage (Node.js)
AlchemyLanguage can detect general concepts referenced in your content. The service returns [Linked Data][1] links for each concept and a URL to a relevant website when possible.

> This example requires [AlchemyLanguage service credentials][2] and [Node.js][3]

1. Use a command-line interface to install the [Watson Developer Cloud Node.js SDK][4]:


    $ npm install watson-developer-cloud

2. Save the following code into an *app.js* file in the same directory. Make sure you replace `API_KEY` with your AlchemyAPI key.

<!-- language: lang-js -->

```
var AlchemyLanguageV1 = require('watson-developer-cloud/alchemy-language/v1');
var alchemy_language = new AlchemyLanguageV1({
  api_key: 'API_KEY'
})

var parameters = {
  url: 'http://www.cnn.com'
};

alchemy_language.concepts(parameters, function (err, response) {
  if (err)
    console.log('error:', err);
  else
    console.log(JSON.stringify(response, null, 2));
});
``` 
3. Run the app:


    $ node app.js


  [1]: http://linkeddata.org/
  [2]: https://www.wikiod.com/ibm-watson-cognitive/getting-started-with-ibm-watson-cognitive#Getting API credentials#t=201609142342226653189
  [3]: https://nodejs.org/
  [4]: https://github.com/watson-developer-cloud/node-sdk/

