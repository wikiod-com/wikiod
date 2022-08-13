---
title: "Differences between sending to Android and iOS devices"
slug: "differences-between-sending-to-android-and-ios-devices"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Make device receive notification even when sleeping
When sending a notification to an iOS device, you must set `priority: "high"` for it to wake up. Otherwise, the notification will not be received while the phone is asleep.


> Sets the priority of the message. Valid values are "normal" and "high." On iOS, these correspond to APNs priorities 5 and 10.
> 
> By default, messages are sent with normal priority. Normal priority optimizes the client app's battery consumption and should be used unless immediate delivery is required. For messages with normal priority, the app may receive the message with unspecified delay.
> 
> When a message is sent with high priority, it is sent immediately, and the app can wake a sleeping device and open a network connection to your server.

--- [FCM Server Reference](https://firebase.google.com/docs/cloud-messaging/http-server-ref)

