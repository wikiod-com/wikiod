---
title: "Navigator Object"
slug: "navigator-object"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
 - var userAgent = navigator.userAgent; /* It can simply be assigned to a variable */

1. There is no public standard for the **`Navigator`** object, however, all major browsers support it.

2. The **`navigator.product`** property cannot be considered a reliable way to get the browser's engine name since most browsers it will return **`Gecko`**. Additionally, it is not supported in:

    - Internet Explorer 10 and below 
    - Opera 12 and greater

3. In Internet Explorer, the **`navigator.geolocation`** property is not supported in versions older than IE 8

4. The **`navigator.appCodeName`** property returns **`Mozilla`** for all modern browsers.

## Get some basic browser data and return it as a JSON object
The following function can be used to get some basic information about the current browser and return it in JSON format.


    function getBrowserInfo() {
        var
            json = "[{",
            
            /* The array containing the browser info */
            info = [
                navigator.userAgent, // Get the User-agent
                navigator.cookieEnabled, // Checks whether cookies are enabled in browser
                navigator.appName, // Get the Name of Browser
                navigator.language,  // Get the Language of Browser
                navigator.appVersion,  // Get the Version of Browser
                navigator.platform  // Get the platform for which browser is compiled
            ],

            /* The array containing the browser info names */
            infoNames = [
                     "userAgent",
                     "cookiesEnabled",
                     "browserName",
                     "browserLang",
                     "browserVersion",
                     "browserPlatform"
            ];
    
        /* Creating the JSON object */
        for (var i = 0; i < info.length; i++) {
            if (i === info.length - 1) {
                json += '"' + infoNames[i] + '": "' + info[i] + '"';
            }
            else {
                json += '"' + infoNames[i] + '": "' + info[i] + '",';
            }
        };

        return json + "}]";
    };

