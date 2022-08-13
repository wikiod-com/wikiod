---
title: "Invalidating Json Web Tokens"
slug: "invalidating-json-web-tokens"
draft: false
images: []
weight: 9861
type: docs
toc: true
---

There are several reason to invalidate a JWT token before its expiration time: account deleted/blocked/suspended, password or permissions changed, user logged out by admin. 

JWT is self-contained, signed and stored outside of the server context, so revoking a token is not a simple action.


## Remove the token from client storage
Remove the token from the client storage to avoid usage

Tokens are issued by the server and you can not force browsers to delete a cookie/localStorage or control how external clients are managing your tokens. Obviously **if attackers have stolen the token before logout** they still could use the token, therefore **are needed additional measures in server side** (see below for token blacklist strategy)

Cookies
-------
You cannot force browsers to delete a cookie. The client can configure the browser in such a way that the cookie persists, even if it's expired. But the server can set the value to empty and include expires field to invalidate the cookie value.

     Set-Cookie: token=deleted; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT

Delete 'token' with javascript
-------

    document.cookie = 'token=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
    localStorage.removeItem('token')
    sessionStorage.removeItem('token')


## Token blacklist
Mark invalid tokens, store until their expiration time and check it in every request. 

Blacklist breaks JWT statelessness because it requires maintaining the state. One of the benefits of JWT is no need server storage, so if you need to revoke tokens without waiting for the expiration, think also about the downside

## Manage the blacklist ##

The blacklist can be easily managed in your own service/database. The storage size probably would not be large because it is only needed to store tokens that were between logout and expiry time. 

Include the full token or just the unique ID `jti`. Set the `iat` (issued at) to remove old tokens.

To revoke all tokens after updating critical data on user (password, permissions, etc) set a new entry with `sub` and `iat` when `currentTime - maxExpiryTime < last iss`​. The entry can be discarded when `currentTime - maxExpiryTime > lastModified` (no more non-expired tokens sent). 

## Rotate tokens
Set **expiration time short and rotate tokens**. Issue a new **access token** every few request. Use **refresh tokens** to allow your application to obtain new access tokens without needing to re-authenticate 

Refresh and access tokens
-------------------------

- **access token**: Authorize access to a protected resource. Limited lifetime. Must be kept secret, security considerations are less strict due to their shorter life.

- **Refresh token**: Allows your application to obtain new access tokens without needing to re-authenticate. Long lifetime. Store in secure long-term storage

Usage recomendations:

 - **Web applications**: refresh the access token before it expires, each time user open the application and at fixed intervals. Alternatively renew the access token when a user performs an action. If the user uses an expired access token, the session is considered inactive and a new access token is required. This new token can be obtained with a refresh token or requiring credentials

 - **Mobile/Native applications**: Application login once and only once. Refresh token does not expire and can be exchanged for a valid JWT. Take in account special events like changing password



## Other common techniques
- Allow change user unique ID if account is compromised with a new user&password login

 - To invalidate tokens when user changes their password or permissions, sign the token with a hash of those fields. If any of these field change, any previous tokens automatically fail to verify. The downside is that it requires access to the database

 - Change signature algorithm to revoke all current tokens in a major security issue

