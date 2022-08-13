---
title: "Calling Twitch APIs"
slug: "calling-twitch-apis"
draft: false
images: []
weight: 9909
type: docs
toc: true
---

This topic is meant to show a general way to call the Twitch API without OAuth. You can call any APIs found in the [Twitch REST API documentation][1] using this pattern. You would simply change the URL to the correct endpoint.

A Client-ID is required for all calls to the Twitch API. In these examples, the Client-ID is added as a header to each call. You can also add it with the `client_id` query string parameter. If you use an OAuth token, the Twitch API will automatically resolve the Client-ID for you. 

You can register a developer application at the [new client page on Twitch][2].


  [1]: https://github.com/justintv/Twitch-API/tree/master/v3_resources
  [2]: https://www.twitch.tv/kraken/oauth2/clients/new

## PHP
The following will retrieve a `channel` object for the `twitch` channel and echo the response.
 
      $channelsApi = 'https://api.twitch.tv/kraken/channels/';
       $channelName = 'twitch';
       $clientId = '...';
       $ch = curl_init();
     
       curl_setopt_array($ch, array(
          CURLOPT_HTTPHEADER=> array(
          'Client-ID: ' . $clientId
          ),
          CURLOPT_RETURNTRANSFER=> true,
          CURLOPT_URL => $channelsApi . $channelName
       ));
     
       $response = curl_exec($ch);
       curl_close($ch);
       echo $response;

## JavaScript
The following will log the JSON response from the API to the console if the request was successful, otherwise it will log the error.

    var xhr = new XMLHttpRequest();
      
    xhr.open('GET', 'https://api.twitch.tv/kraken', true);
      
    xhr.setRequestHeader('Client-ID', '...');
      
    xhr.onload = function(data){
      console.log(data);
    };
      
    xhr.onerror = function(error){
      console.log(error.target.status);
    };
      
    xhr.send();

## jQuery
The following will retrieve a `channel` object for the `twitch` channel. If the request was successful the `channel` object will be logged to the console.

    $.ajax({
      type: 'GET',
      url: 'https://api.twitch.tv/kraken/channels/twitch',
      headers: {
        'Client-ID': '...'
      },
      success: function(data) {
        console.log(data);
      }
    });

