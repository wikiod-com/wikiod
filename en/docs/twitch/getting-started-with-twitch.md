---
title: "Getting started with twitch"
slug: "getting-started-with-twitch"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Get the OAuth token from the URL fragment
If the user authorizes your application, they will be redirected to the following URL:

    https://[your registered redirect URI]/#access_token=[an access token]
            &scope=[authorized scopes]

Note that the access token is in the URL fragment and not the query string. This means the value will not show up in HTTP requests to your server. URL fragments can be accessed from JavaScript with `document.location.hash`.

## Requesting a token
The Implicit Grant flow is best suited for Web applications. It's easily integrated into a website using JavaScript and doesn't require a server to store the authorization code to retrieve a token.

You'll first send the user to the Twitch authorization endpoint. This URL is made up of a the base authorization URL (`https://api.twitch.tv/kraken/oauth2/authorize`) and query string parameters that define what you're requesting. The required parameters are `response_type`, `client_id`, `redirect_uri`, and `scope`.
 
For the Implicit Grant flow, the `response_type` parameter is always set to `token`. This signifies that you're requesting an OAuth token directly.
 
The `redirect_uri` is where the user will be redirected after they approve the scopes your application requested. This must match what you registered on your Twitch account [Connections page][1].
 
The `client_id` is a unique identifier for your application. You can find your client ID on the Connections page, too.
 
The `scope` parameter defines what you have access to on behalf of the user. You should only request the minimum that you need for your application to function. You can find the list of scopes on the [Twitch API GitHub][2].

The `state` parameter is also supported to help protect against cross-site scripting attacks. When the user is redirected after authorization, this value will be included on the `redirect_uri`.

Redirect the user to this URL:

    https://api.twitch.tv/kraken/oauth2/authorize
        ?response_type=token
        &client_id=[your client ID]
        &redirect_uri=[your registered redirect URI]
        &scope=[space separated list of scopes]
        &state=[your provided unique token]

 
 [1]: https://www.twitch.tv/settings/connections
 [2]: https://github.com/justintv/Twitch-API/blob/master/authentication.md#scopes

