---
title: "OAuth 2.0 Client"
slug: "oauth-20-client"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntax
- key1=value1&keyN=valueN

## Parameters
| Parameter | Value |
| ------ | ------ |
Parameters used with the `token` endpoint: | . |
**`grant_type`**   | `client_credentials`, `password`, `authorization_code`, `refresh_token`  |
**`client_id`** | OAuth application's ID |
**`client_secret`** | OAuth application's Secret |
**`redirect_uri`** | URL encoded absolute URL to the user's server |
**`code`** | used only with the `authorization_code` grant |
**`username`** | used only the `password` grant |
**`password`** | used only the `password` grant |
**`refresh_token`** | used only with the `refresh_token` grant |
Parameters used with the `authorization` endpoint of the `authorization_code` grant: | . |
**`state`** | random state string |
**`scope`** | list of scopes separated by either a comma or an empty space | 


## Client Credentials Grant
    POST /token HTTP/1.1
    Host: server.example.com
    Content-Type: application/x-www-form-urlencoded

    grant_type=client_credentials&client_id=[APP_KEY]&client_secret=[APP_SECRET]

[*Source*](https://tools.ietf.org/html/rfc6749#section-4.4)

## Resource Owner Password Credentials Grant
    POST /token HTTP/1.1
    Host: server.example.com
    Content-Type: application/x-www-form-urlencoded

    grant_type=password&username=[USERNAME]&password=[PASSWORD]
        &client_id=[APP_KEY]&client_secret=[APP_SECRET]

[*Source*](https://tools.ietf.org/html/rfc6749#section-4.3)

## Authorization Code Grant
# Step 1

    GET /authorize?response_type=code&client_id=[APP_KEY]&state=[RANDOM_STRING]
        &redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb
        &scope=[OPTIONAL_SCOPES] HTTP/1.1
    Host: server.example.com

# Step 2

    POST /token HTTP/1.1
    Host: server.example.com
    Content-Type: application/x-www-form-urlencoded

    grant_type=authorization_code&code=[CODE_FROM_STEP1]
        &client_id=[APP_KEY]&client_secret=[APP_SECRET]
        &redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb

[*Source*](https://tools.ietf.org/html/rfc6749#section-4.1)

## Refreshing an Access Token
    POST /token HTTP/1.1
    Host: server.example.com
    Content-Type: application/x-www-form-urlencoded

    grant_type=refresh_token&refresh_token=[REFRESH_TOKEN]
        &client_id=[APP_KEY]&client_secret=[APP_SECRET]

[*Source*](https://tools.ietf.org/html/rfc6749#section-6)

## Implicit Grant
    
    Host: server.example.com
    HTTP/1.1
    
    GET /authorize?response_type=token&client_id=[APP_KEY]&state=[OPTIONAL_STATE]
        &redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb&scope=[OPTIONAL_SCOPES]
   
[Source][1]


  [1]: https://tools.ietf.org/html/rfc6749#section-4.2

