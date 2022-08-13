---
title: "Firebase Cloud Messaging"
slug: "firebase-cloud-messaging"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

**Firebase Cloud Messaging** (FCM) is a cross-platform messaging solution that lets you reliably deliver messages at no cost.

Using FCM, you can notify a client app that new email or other data is available to sync. You can send notification messages to drive user reengagement and retention. For use cases such as instant messaging, a message can transfer a payload of up to 4KB to a client app.

Official Documentation: https://firebase.google.com/docs/cloud-messaging/

## Sending Downstream Messages using Postman
To do this in [Postman][1], you simply have to set the following:

1. Set request type to `POST`
2. In the *Headers*, set the following:
    - Content-Type = application/json
    - Authorization = < Your FCM Server Key > (See your [Firebase Console's Cloud Messaging Tab][2])
3. Set the payload parameters in the *Body* (*in this example, we used the *raw* option, see screenshot (2)*)
4. Send the request to https://fcm.googleapis.com/fcm/send

Screenshots:

(1)
[![enter image description here][3]][3]

**Note**: Always keep your Server Key a secret. Only a portion of my key is visible here so it should be fine.

(2)
[![enter image description here][4]][4]

(3)
[![enter image description here][5]][5]

Notice that the request was a success with the `message_id` in the response.


  [1]: https://chrome.google.com/webstore/detail/postman/fhbjgbiflinjbdggehcddcbncdddomop?hl=en
  [2]: https://console.firebase.google.com/project/_/settings/cloudmessaging
  [3]: https://i.stack.imgur.com/Ppptt.png
  [4]: https://i.stack.imgur.com/eQdY5.png
  [5]: https://i.stack.imgur.com/aZhso.png

## Sending Downstream Messages via cURL
You can test sending messages using the FCM REST API by sending a request through cURL.

    curl --header "Authorization: key=<API_KEY>" \
           --header Content-Type:"application/json" \
           https://fcm.googleapis.com/fcm/send \
           -d "{\"registration_ids\":[\"ABC\"]}"

<sub> Syntax retrieved from [here][1].</sub>

The `API_KEY` indicated above is referring to the Server Key that can be seen in your [Firebase Console's Cloud Messaging Tab][2].

The part where:

    "{\"registration_ids\":[\"ABC\"]}"

is, can be replaced with your own payload. See the [FCM HTTP Protocol Documentation][3] for more details.


  [1]: https://firebase.google.com/docs/cloud-messaging/server#checkAPIkey
  [2]: https://console.firebase.google.com/project/_/settings/cloudmessaging
  [3]: https://firebase.google.com/docs/cloud-messaging/http-server-ref

