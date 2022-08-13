---
title: "Client calls to Google apps-script"
slug: "client-calls-to-google-apps-script"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Google appscript runs well as a stand alone platform and in the addon format for Google docs, sheets and forms. However, there are times when a client browser may need to call to a Google app to perform some action. 

Therefore, Google introduced client side requests to Google apps-scripts. To solve this problem, Google introduced the [client side libraries](https://developers.google.com/api-client-library/javascript/start/start-js)

## This is an example of a client side call to a Google app-script
    
    <script src="https://apis.google.com/js/api.js"></script>
    <script>
    function start() {
     // 2. Initialize the JavaScript client library.
     gapi.client.init({
    'apiKey': 'YOUR_API_KEY',
    // clientId and scope are optional if auth is not required.
    'clientId': 'YOUR_WEB_CLIENT_ID.apps.googleusercontent.com',
    'scope': 'profile',
    }).then(function() {
    // 3. Initialize and make the API request.
    return gapi.client.request({
      'path': 'https://people.googleapis.com/v1/people/me',
      })
     }).then(function(response) {
    console.log(response.result);
    }, function(reason) {
    console.log('Error: ' + reason.result.error.message);
    });
    };
    // 1. Load the JavaScript client library.
     gapi.load('client', start);
    </script>

