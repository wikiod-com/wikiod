---
title: "Getting started with Dropbox API"
slug: "getting-started-with-dropbox-api"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting an OAuth 2 access token for the Dropbox API via the code grant using curl
Abbreviated from https://blogs.dropbox.com/developers/2013/07/using-oauth-2-0-with-the-core-api/:

**Step 1: Begin authorization**

Send the user to this web page, with your values filled in:

    https://www.dropbox.com/oauth2/authorize?client_id=<app key>&response_type=code&redirect_uri=<redirect URI>&state=<CSRF token>

The authorization code will be included as the `code` parameter on the redirect URI.

**Step 2: Obtain an access token**

    curl https://api.dropbox.com/oauth2/token -d code=<authorization code> -d grant_type=authorization_code -d redirect_uri=<redirect URI> -u <app key>:<app secret>

**Step 3: Call the API**

In your API call, set the header:

    Authorization: Bearer <access token>

Check out the [blog post][1] for more details, including an important security note on using `state` to protect against CSRF attacks.


  [1]: https://blogs.dropbox.com/developers/2013/07/using-oauth-2-0-with-the-core-api/

