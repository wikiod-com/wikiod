---
title: "Getting started with youtube-api"
slug: "getting-started-with-youtube-api"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting started with YouTube APIs
There are currently three YouTube APIs available to the public:

1. YouTube Data API
2. YouTube Analytics API
3. YouTube Reporting API

Each of these offer different functionality and are treated as separate, individual APIs.

Since YouTube is a subsidiary of Google, the various YouTube APIs are provided and maintained by Google. In order to access any Google-provided API, following steps are necessary:

1. You will need a Google Account to access the Google Developers Console.
2. Create a new project in your Google Developers Console.
3. You then are able to request an API key, and register your application.
4. The required API's that you will need access to will need to be enabled in the [API library][1] of the Google Cloud Console.

When On the [credentials screen][2], an API key has to be generated. The type of key that is needed depends on the individual application.

There are five types of API keys:

| Key type | Description | Use case / language |
| ------ | ------ | ------ |
| None| Has no restrictions and is normally used for testing and development| open to all environments|
| Server key | Used for server-side applications. The key is considered a secret and may not be exposed to the public.  | PHP, Java, Python, Ruby, C, etc. |
| Browser key | Used for client-side applications. Since the client is going to issue requests to the API, the key **cannot** be a secret. | JavaScript |
| Android key | For use within an Android app. | Android app |
| iOS key | For use within an iOS app. | iOS app |

Once an API key is obtained and configured, you can then use it to make calls to the API.

For general API call's, all you need is the API Key.  
But if you need to access a user's account to gain more information, upload or generally modify a user's account data, you will need a "Client ID" and a "Client secret".  
These are used for authentication with the [OAuth2][3] framework.


  [1]: https://console.cloud.google.com/apis/library
  [2]: https://console.cloud.google.com/apis/credentials
  [3]: https://developers.google.com/identity/protocols/OAuth2

## YouTube Data API
This API, sometimes also referred to as "API v3", "YouTube Data API v3" or just "YouTube API", is the most commonly used YouTube API. It enables an application to read, alter, add or delete data related to videos, playlists and channels. This includes, but is not limited to:

1. Performing full-text searches for videos, channels and/or playlists
1. Retrieving information about a video, channel or playlist by id
1. Uploading videos
1. Editing a video's, channel's or playlist's description, visibility and other information
1. Managing a channels profile picture, banner and other information
1. Reading, posting and editing comments

## YouTube Analytics API & YouTube Reporting API
These APIs are used to query for video and channel analytics, like views/clicks and votes.

