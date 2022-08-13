---
title: "Salesforce REST API"
slug: "salesforce-rest-api"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Force.com REST API Documentation. Full list of API's is [here](https://resources.docs.salesforce.com/sfdc/pdf/api_rest.pdf)

## OAuth2 access_token and list of services
To get OAuth2 access token simply do <br>
`curl https://login.salesforce.com/services/oauth2/token -d "grant_type=password" -d "client_id=myclientid" -d "client_secret=myclientsecret" -d "username=mylogin@salesforce.com" -d "password=mypassword123456"`

You should get response something like <br>

    {
      "access_token": "00D6F0xxx001g1qs!ARsAQL7BRiQQ0lgTW7zXu3kILJBxxxxxHvDnChF2ETBFJpX0T2LsBsm8MVABhAvINAyZqgDIAHhJDp6QjuF6ZAYFE",
      "instance_url": "https://ap4.salesforce.com",
      "id": "https://login.salesforce.com/id/00D6F000001xxxxAA/0056F000006DMcxxxx",
      "token_type": "Bearer",
      "issued_at": "14878401xxxxx",
      "signature": "Ra5Sdm6gq4xxxeZYk3H2yBIVpZ6hBUDgkQ4Tjp9Q="
    }

Then do<br>

    {
      "tooling": "/services/data/v20.0/tooling",
      "eclair": "/services/data/v20.0/eclair",
      "prechatForms": "/services/data/v20.0/prechatForms",
      "async-queries": "/services/data/v20.0/async-queries",
      "query": "/services/data/v20.0/query",
      "chatter": "/services/data/v20.0/chatter",
      "wave": "/services/data/v20.0/wave",
      "search": "/services/data/v20.0/search",
      "identity": "https://login.salesforce.com/id/00D6F000001g1qsUAA/0056F000006DMcMQAW",
      "sobjects": "/services/data/v20.0/sobjects",
      "serviceTemplates": "/services/data/v20.0/serviceTemplates",
      "recent": "/services/data/v20.0/recent",
      "connect": "/services/data/v20.0/connect",
      "licensing": "/services/data/v20.0/licensing"
    }

