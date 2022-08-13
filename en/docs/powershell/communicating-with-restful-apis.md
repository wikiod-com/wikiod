---
title: "Communicating with RESTful APIs"
slug: "communicating-with-restful-apis"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

REST stands for Representational State Transfer (sometimes spelled "ReST"). It relies on a stateless, client-server, cacheable communications protocol and mostly HTTP protocol is used. It is primarily used to build Web services that are lightweight, maintainable, and scalable. A service based on REST is called a RESTful service and the APIs which are being used for it are RESTful APIs. In PowerShell, *`Invoke-RestMethod`* is used to deal with them.

## Post Message to hipChat
```
$params = @{
    Uri = "https://your.hipchat.com/v2/room/934419/notification?auth_token=???"
    Method = "POST"
    Body = @{
        color = 'yellow'
        message = "This is a test message!"
        notify = $false 
        message_format = "text"
    } | ConvertTo-Json
    ContentType = 'application/json'
}

Invoke-RestMethod @params 


## Using REST with PowerShell Objects to GET and POST many items
GET your REST data and store in a PowerShell object:

    $Users = Invoke-RestMethod -Uri "http://jsonplaceholder.typicode.com/users"

Modify many items in your data:

    $Users[0].name = "John Smith"
    $Users[0].email = "John.Smith@example.com"
    $Users[1].name = "Jane Smith"
    $Users[1].email = "Jane.Smith@example.com"

POST all of the REST data back:

    $Json = $Users | ConvertTo-Json
    Invoke-RestMethod -Method Post -Uri "http://jsonplaceholder.typicode.com/users" -Body $Json -ContentType 'application/json'

## Use Slack.com Incoming Webhooks
Define your payload to send for possible more complex data

```
$Payload = @{ text="test string"; username="testuser" }
```

Use `ConvertTo-Json` cmdlet and `Invoke-RestMethod` to execute the call 

```
Invoke-RestMethod -Uri "https://hooks.slack.com/services/yourwebhookstring" -Method Post -Body (ConvertTo-Json $Payload) 
```

## Using REST with PowerShell Objects to Get and Put individual data
GET your REST data and store in a PowerShell object:

    $Post = Invoke-RestMethod -Uri "http://jsonplaceholder.typicode.com/posts/1"

Modify your data:

    $Post.title = "New Title"

PUT the REST data back

    $Json = $Post | ConvertTo-Json
    Invoke-RestMethod -Method Put -Uri "http://jsonplaceholder.typicode.com/posts/1" -Body $Json -ContentType 'application/json'


## Using REST with PowerShell to Delete items
Identify the item that is to be deleted and delete it:

    Invoke-RestMethod -Method Delete -Uri "http://jsonplaceholder.typicode.com/posts/1"

