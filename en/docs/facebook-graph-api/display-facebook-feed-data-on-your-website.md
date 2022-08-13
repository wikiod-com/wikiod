---
title: "Display Facebook feed, data on your website"
slug: "display-facebook-feed-data-on-your-website"
draft: false
images: []
weight: 9870
type: docs
toc: true
---

## Getting FaceBook Access Token to read and write to the Facebook social graph
This document details the steps to obtain the facebook access tokens and the using the tokens to fetch FB feeds.

**Example:**
A live example is available in 

> https://newtonjoshua.com

**Introduction to Graph API:**
The Graph API is the primary way to get data in and out of Facebook's platform. It's a low-level HTTP-based API that you can use to query data, post new stories, manage ads, upload photos and a variety of other tasks that an app might need to do.

**FaceBook Apps:**

> https://developers.facebook.com

Create a Facebook app. You will get an <code>App_Id</code> and <code>App_Secret</code>

**Graph API Explorer:**

> https://developers.facebook.com/tools/explorer/<code>**{{App_Id}}**</code>/?method=GET&path=me%2Ffeed&version=v2.8

You will get an `access_token` which is short lived. So this will be our `short_lived_access_token`.

note: while creating access token select all the fb fields that you require.This will give permission to the access token to fetch those fields. 

**Access Token Extension:**

> https://graph.facebook.com/oauth/access_token?grant_type=fb_exchange_token&client_id=**{{App_Id}}**&client_secret=**{{App_Secret}}**&fb_exchange_token=**{{short-lived-access_token}}**

You will get an `access_token` with a validity of 2 months.

**Access Token Debugger:**

> https://developers.facebook.com/tools/debug/accesstoken?q=**{{access_token}}**&version=v2.8

you can check check the details of the `access_token`.

**Facebook SDK for JavaScript:**
Include the below JavaScript in your HTML to asynchronously load the SDK into your page

    <script>
            (function (d, s, id) {
                var js, fjs = d.getElementsByTagName(s)[0];
                if (d.getElementById(id)) {
                    return;
                }
                js = d.createElement(s);
                js.id = id;
                js.src = "//connect.facebook.net/en_US/sdk.js";
                fjs.parentNode.insertBefore(js, fjs);
            }(document, 'script', 'facebook-jssdk'));
        </script>


**Graph API:**
Let's make an API call to get our FB id, profile pic, cover pic and feeds.

    <script>
    window.fbAsyncInit = function () {
        FB.init({
            appId: '{{App_Id }}',
            xfbml: true,
            version: 'v2.7'
        });
        FB.api(
            '/me',
            'GET', {
                fields: 'id,picture{url},cover,feed',
                access_token: {{access_token}}
            },
            function (response) {
            if (response.error) {
                    console.error(response.error.message);
                }
                if (response.picture.data.url) {
                    profilePic = response.picture.data.url;
                }
                if (response.cover.source) {
                    coverPic = response.cover.source;
                }
                if (response.feed.data) {
            feeds = response.feed.data;
                    feeds.forEach(function (feed) {
                // view each feed content
                    });
                }
                if (response.feed.paging.next) {
                    nextFeedPage = response.feed.paging.next;
            // a request to nextFeedPage will give the next set of feeds
                }
                
            }
        );
    };
    
    </script>

Use the Graph API Explorer to design your querry that should be entered in the 'fields' (eg: 'id,picture{url},cover,feed')

Now you can fetch your facebook data from Facebook Graph API using your access_token.
refer https://developers.facebook.com/docs/graph-api/overview/

note: Your `access_token` will expire in 2 months. Create a new access_token after that.

