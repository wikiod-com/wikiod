---
title: "Send an SMS message using Bash and cURL"
slug: "send-an-sms-message-using-bash-and-curl"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
 - https://api.twilio.com/{Api
   version}/Accounts/{AccountSid}/Messages.json {body}

## Parameters
| Parameter | Details |
| ------ | ------ |
| {Api version} | Versioning date of the API. e.g. 2010-04-01  |
| {Account Sid} | The Account Sid. Starts AC |
| {body} | URL encoded body including From, To, Body and MessagingServiceSid |

## Send a single message using an alphanumeric sender id
An HTTP POST request is sent to a URL of the format: "https://api.twilio.com/2xxx-xx-xx/Accounts/[AccountSid]/Messages.json

The example below uses a alphanumeric string as the sender. At the time of writing a sender ID can only be added through a service request Twlio.

Example Request:

    To="+447111111111" ;
    From="Wxxxxxxxxx" ;
    MessagingServiceSid="MGxxxxxxxxxxxxxxxxxxxxxx" ;
    Body="Test Message" ;
    AccountSid="ACxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
    AuthToken="[Auth Token]" ;
    CMD="curl --request POST \"https://api.twilio.com/2010-04-01/Accounts/${AccountSid?}/Messages.json\" " ;
    CMD="${CMD?}--data-urlencode \"From=${From?}\" " ;
    CMD="${CMD?}--data-urlencode \"To=${To?}\" " ;
    CMD="${CMD?}--data-urlencode \"MessagingServiceSid=${MessagingServiceSid?}\" " ;
    CMD="${CMD?}--data-urlencode \"Body=${Body?}\" " ;
    CMD="${CMD?}-u \"${AccountSid?}:${AuthToken?}\"" ;
    echo "${CMD?}" ;
    eval "${CMD?}" ;

Example Response (JSON, formatted):

    {  
       "sid":"SMxxxxxxxxxxxxx",
       "date_created":"Tue, 26 Jul 2016 12:50:07 +0000",
       "date_updated":"Tue, 26 Jul 2016 12:50:07 +0000",
       "date_sent":null,
       "account_sid":"ACxxxxxxxxxx",
       "to":"+447111111111",
       "from":"Wxxxxxxxxx",
       "messaging_service_sid":"MGxxxxxxxxxxx",
       "body":"Test Message",
       "status":"accepted",
       "num_segments":"0",
       "num_media":"0",
       "direction":"outbound-api",
       "api_version":"2010-04-01",
       "price":null,
       "price_unit":null,
       "error_code":null,
       "error_message":null,
       "uri":"/2010-04-01/Accounts/ACxxxxxxxxxxxx/Messages/SMxxxxxxxxxxx.json",
       "subresource_uris":{  
          "media":"/2010-04-01/Accounts/ACxxxxxxxxx/Messages/SMxxxxxxxxxxxx/Media.json"
       }
    }



