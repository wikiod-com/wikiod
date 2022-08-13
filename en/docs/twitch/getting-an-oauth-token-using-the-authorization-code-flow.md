---
title: "Getting an OAuth token using the Authorization Code Flow"
slug: "getting-an-oauth-token-using-the-authorization-code-flow"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Send the user to the authorize endpoint to get the authorization code
You'll first send the user to the Twitch authorization endpoint. This URL is made up of a the base authorization URL (`https://api.twitch.tv/kraken/oauth2/authorize`) and query string parameters that define what you're requesting. The required parameters are `response_type`, `client_id`, `redirect_uri`, and `scope`.

For the Authorization Code flow, the `response_type` parameter is always set to `code`. This signifies that you're requesting an authorization code from the Twitch API. 

The `redirect_uri` is where the user will be redirected after they approve the scopes your application requested. This must match what you registered on your Twitch account [Connections page][1]. 

The `client_id` is a unique identifier for your application. You can find your client ID on the Connections page, too. 

The `scope` defines what you have access to on behalf of the user. You should only request the minimum that you need for your application to function. You can find the list of scopes on the [Twitch API GitHub][2].

The `state` parameter is also supported to help protect against cross-site scripting attacks. The `state` parameter will be included on the `redirect_uri` when the user authorizes your application.

  [1]: https://www.twitch.tv/settings/connections
  [2]: https://github.com/justintv/Twitch-API/blob/master/authentication.md#scopes


      https://api.twitch.tv/kraken/oauth2/authorize
        ?response_type=code
        &client_id=[your client ID]
        &redirect_uri=[your registered redirect URI]
        &scope=[space separated list of scopes]
        &state=[your provided unique token]

## Get the authorization code from the query string
When the user goes to the authorization endpoint, they will be asked to give your application permission to the scopes that you've requested. They can decline this, so you must make sure to take that into consideration in your code. After they've allowed your application access, the user will be redirected to the URL you specified in `redirect_uri`. The query string will now have a `code` parameter, which is the authorization code that you can exchange for an OAuth token.

    <?php
      $authCode = $_GET['code'];
    ?>

## Exchange the code for the OAuth token
Now that you have an authorization code, you can make a POST to the token endpoint (`https://api.twitch.tv/kraken/oauth2/token`) to get an OAuth token. You will receive a JSON-encoded access token, refresh token, and a list of the scopes approved by the user. You can now use that token to make authenticated requests on behalf of the user.

    <?php
      $authCode = $_GET['code'];
    
      $parameterValues = array(
        'client_id' => '...',
        'client_secret' => '...',
        'grant_type' => 'authorization_code',
        'redirect_uri' => 'http://localhost/',
        'code' => $authCode
      );
    
      $postValues = http_build_query($parameterValues, '', '&');
    
      $ch = curl_init();
        
      curl_setopt_array($ch, array(
        CURLOPT_RETURNTRANSFER => true,
        CURLOPT_URL => 'https://api.twitch.tv/kraken/oauth2/token',
        CURLOPT_POST => 1,
        CURLOPT_POSTFIELDS => $postValues
      ));
                
      $response = curl_exec($ch);
      curl_close($ch);
    
      echo $response;
    ?>

