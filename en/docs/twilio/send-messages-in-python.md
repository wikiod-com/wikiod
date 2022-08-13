---
title: "Send messages in Python"
slug: "send-messages-in-python"
draft: false
images: []
weight: 9897
type: docs
toc: true
---

## Syntax
 - class twilio.rest.resources.Messages(*args, **kwargs)

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| to (str) | The destination phone number. |
| from_ (str) | The phone number sending this message (must be a verified Twilio number) |  
| body (str) | The message you want to send, limited to 160 characters. |  
| status_callback | A URL that Twilio will POST to when your message is processed. |     
| application_sid (str) | The 34 character sid of the application Twilio should use to handle this phone call. |     

Twilio will send SMS messages for long codes (in the US, a regular ten digit number) at a rate of one message per second. For additional throughput, you can use pools of numbers with [Copilot][1] or apply for a [shortcode][2], which can send 30 messages or more per second.


  [1]: https://www.twilio.com/docs/api/rest/sending-messages-copilot
  [2]: https://www.twilio.com/sms/shortcodes

## Sending MMS
You can add an image to your message using the parameter `media_url`.    
   

    # Download the twilio-python library from http://twilio.com/docs/libraries
    from twilio.rest import TwilioRestClient
     
    # Find these values at https://twilio.com/user/account
    account_sid = "ACXXXXXXXXXXXXXXXXX"
    auth_token = "YYYYYYYYYYYYYYYYYY"
    client = TwilioRestClient(account_sid, auth_token)    
    message = client.messages.create(
        to="+12316851234",
        from_="TWILIO_NUMBER",
        body="Hello there, StackOverflow!",
        media_url=[
            'https://demo.twilio.com/owl.png',
            'https://demo.twilio.com/logo.png']) 


  [1]: https://www.twilio.com/docs/python/install

## Sending messages and MMS through Twilio
**Twilio** help build apps that communicate with everyone in the world. Voice & Video, Messaging and Authentication APIs for every application.

You can get an API key for free. 
 
To send a message through Twilio, your application needs to make an HTTP POST request to Twilio with the following things:
1. The phone number you want to send the message to.
2. Twilio number from which you’re attempting to send the message. (Only Twilio message-enabled phone numbers will work)
3. The body of the message.


----------

To send an SMS, make an HTTP POST request to the Messages resource.

        POST https://api.twilio.com/20xx-xx-xx/Accounts/xxxxxxx/Messages


Below is an example code to show how sending messeges with Twilio API will work.

    # Download the twilio-python library from http://twilio.com/docs/libraries
    from twilio.rest import TwilioRestClient

    # Find these values at https://twilio.com/user/account
    account_sid = "ACXXXXXXXXXXXXXXXXX"
    auth_token  = "YYYYYYYYYYYYYYYYYY"
    client      = TwilioRestClient(account_sid, auth_token)

    message = client.messages.create(
        to    = "+12316851234",
        from_ = "+15555555555",
        body  = "Hello there!"
    )

If the Twilio number supports MMS, then you can send and receive MMS also.

Below is code to send MMS through Twilio API.

    message = client.messages.create(
        to    = "+12316851234",
        from_ = "+15555555555",
        body  = "Hello there!",
        media_url=[
            'https://demo.twilio.com/owl.png',
            'https://demo.twilio.com/logo.png'
        ])



## Sending SMS
To send your first SMS with Twilio and Python you'll just need the [Twilio-Python][1] helper library to get started.    

    # Download the twilio-python library from http://twilio.com/docs/libraries
    from twilio.rest import TwilioRestClient
     
    # Find these values at https://twilio.com/user/account
    account_sid = "ACXXXXXXXXXXXXXXXXX"
    auth_token = "YYYYYYYYYYYYYYYYYY"
    client = TwilioRestClient(account_sid, auth_token)
     
    message = client.messages.create(to="+12316851234", from_="TWILIO_NUMBER",
                                     body="Hello there, StackOverflow!") 

[1]: https://github.com/twilio/twilio-python

