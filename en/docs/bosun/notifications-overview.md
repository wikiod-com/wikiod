---
title: "Notifications Overview"
slug: "notifications-overview"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Syntax
 - notification *name* {
   - email = dev-alerts@example.com, prod-alerts@example.com, ...
   - post = http://example.com
   - get = http://example.com
   - next = another-notification-definition
   - timeout = 30m
   - runOnActions = false
   - body = {"text": {{.|json}}}
   - contentType = application/json
   - print = true
- }

In Bosun notifications are used for both new alert incidents and when an alert is acked/closed/etc. If you don't want the other events to trigger a notification add `runOnActions = false` to the notification definition.

See also:

 - [Notifications: Chat Systems][1]


  [1]: https://www.wikiod.com/bosun/notifications--chat-systems

## HTTP GET/POST Notifications
Alert incidents can be sent to other system using HTTP GET or HTTP POST requests. You can either send the rendered alert directly (using markdown in the template perhaps) or use `body = ... {{.|json}} ...` and contentType to send the alert data over as part of a JSON object. Another approach is to only send the basic incident information and then have the receiving system pull additional details from the bosun API.

```
notification postjson {
    post = ${env.POSTURL}
    body = {"text": {{.|json}}, apiKey=${env.APIKEY}}
    contentType = application/json
}
```
The contentType for HTTP GET/POST requests is **application/x-www-form-urlencoded** by default.

## SMS Notifications with plivo
There are two ids you will need from your [plivo](https://www.plivo.com/sms-api/) account. Replace `authid` and `authtoken` in this snippet with those values. The `src` value should also be a valid number assigned to your account. `dst` can be any number you want, or multiple seperated by `<`.

```
notification sms {
  post = https://authid:authtoken@api.plivo.com/v1/Account/authid/Message/
  body = {"text": {{.|json}}, "dst":"15551234567","src":"15559876543"}
  contentType = application/json
  runOnActions = false
}
```

This will text the alert subject to all numbers in `dst`.

## Email Notifications
To send email notifications you need to add the following settings to your config file:

```
#Using a company SMTP server (note only one can be define)
smtpHost = mail.example.com:25
emailFrom = bosun@example.com

#Using Gmail with TLS and username/password
smtpHost = smtp.gmail.com:587
emailFrom = myemail@gmail.com
smtpUsername = myemail@gmail.com
smtpPassword = ${env.EMAILPASSWORD}

#Chained notifications will escalate if an incident is not acked before the timeout
notification it {
    email = it-alerts@example.com
    next = oncall
    timeout = 30m
}

#Could set additional escalations here using any notification type (email/get/post)
#or set next = oncall to send another email after the timeout if alert is still not acked
notification oncall {
    email = escalated-alerts@example.com
}

#Multiple email addresses can be specified in one notification definition
#Use runOnActions = false to exclude updates on actions (ack/closed/forget)
notification engineering {
    email = core-alerts@example.com,qa-alerts@example.com,prod-alerts@example.com
    runOnActions = false
}
```

## Overview
[Bosun notifications][1] are assigned to alert definitions using warnNotification and critNotification and indicate where to send the rendered alert template when a new incident occur. Notifications can be sent via email or use HTTP GET/POST requests. There also is a Print notification that just adds information to the Bosun log file.

If you want to hide a URL, Password, or API Key from being in plain text you can use `${env.VARIABLENAME}` to load the value from an Environmental Variable (usually exported from the Bosun init script). Please note that there are no protections on who can access the variables (they can easily be displayed in a template) but it does prevent them from being displayed directly on the Rule Editor page or in the .conf file.

```
notification logfile {
    print = true
}

#print can be added to any notification type to help with debugging
notification email {
    email = sysadmins@example.com
    print = true
}
```

  [1]: https://bosun.org/configuration#notification

## SMS Notifications with Twilio
Swap out `AccountSid`, `AuthToken`, `ToPhoneNumber` and `FromPhoneNumber` for your credentials/intended recipients. You need to ensure that if the `ToPhoneNumber` and `FromPhoneNumber` have + in them, they are urlencoded (ie: as %2B)

```
notification sms {
    post = https://{AccountSid}:{Authtoken}@api.twilio.com/2010-04-01/Accounts/{AccountSid}/Messages.json
    body = Body={{.}}&To={ToPhoneNumber}&From={FromPhoneNumber}
}
```
_From gist:_ https://gist.github.com/aodj/58535c4c152b6073eaf5

## PagerDuty Notifications
    #Post to pagerduty.com
    notification pagerduty {
        post = https://events.pagerduty.com/generic/2010-04-15/create_event.json
        contentType = application/json
        runOnActions = false
        body = `{
         "service_key": "myservicekey",
         "incident_key": {{.|json}},
         "event_type": "trigger",
         "description": {{.|json}},
         "client": "Bosun",
         "client_url": "http://bosun.example.com/"
        }`
    }



## Changing Notification Using Lookup
In some cases you may want to change which notification you use based on a tag in the Alert keys. You can do this using the [Lookup][1] feature. Note: Lookup only works if you are using OpenTSDB and sending data to the Bosun to be indexed. For other backends or non-indexed data you have to use lookupSeries instead.

    notification default {
        email = team@example.com
    }
    
    notification JSmith{
        email = JSmith@example.com
    }
    
    #This will use the JSmith lookup for any alerts where the host tag starts with ny-jsmith
    lookup host_base_contact {
        entry host=ny-jsmith* {
            main_contact = JSmith
        }
        entry host=* {
            main_contact = default
        }
    }
    
    alert blah {
        ...
        warn = q(...)
        warnNotification = lookup("host_base_contact", "main_contact")
        critNotification = lookup("host_base_contact", "main_contact")
    }

This can also be applied to multiple alerts using [Macros][2]:

    macro host.based.contacts {
        warnNotification = lookup("host_base_contact", "main_contact")
        critNotification = lookup("host_base_contact", "main_contact")
    }


  [1]: http://bosun.org/expressions#lookuptable-string-key-string-numberset
  [2]: http://bosun.org/configuration#macro

