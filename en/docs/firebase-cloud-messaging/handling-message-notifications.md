---
title: "Handling Message Notifications"
slug: "handling-message-notifications"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Message handling on Android


## Message handling on iOS


## Message handling with app in background or killed
Firebase handles notifications differently when the app is in background (killed process) and when in foreground (active).

> When your app is in the background, notification messages are displayed in the system tray, and onMessageReceived is not called. For notification messages with a data payload, the notification message is displayed in the system tray, and the data that was included with the notification message can be retrieved from the intent launched when the user taps on the notification.
[[1]]

If the app is in background the service will trigger the notification by default with the <code>title</code> and <code>body</code> in the notification and as mentioned <code>onMessageReceived</code> method will not be trigger. Instead, the the click will open the <code>activity</code> from the <code>Manifest.xml</code> marked with:
```xml
<intent-filter>
        <action android:name="android.intent.action.MAIN"/>
        <category android:name="android.intent.category.LAUNCHER"/>
</intent-filter>
```
From this point forward you can get your <code>data</code> within this activity's <code>intent</code>:

<pre>
if (getIntent() != null && getIntent().getExtras() != null) {
      String customString = (String) getIntent().getExtras().get("myStringData");
      Integer customInteger = (Integer) getIntent().getExtras().get("myIntData");
}
</pre>

Setting icon and icon background color in this case can be done from within <code>Manifest.xml</code>[[2]]:


```
<meta-data
        android:name="com.google.firebase.messaging.default_notification_icon"
        android:resource="@drawable/your_drawable_icon" />

<meta-data
        android:name="com.google.firebase.messaging.default_notification_color"
        android:resource="@color/your_color" />
```
source 1: [FCM background handling][1]

source 2: [FCM github repository][2]


  [1]: https://firebase.google.com/support/faq/#fcm-android-background "FCM foreground"
  [2]: https://github.com/firebase/quickstart-android/tree/master/messaging "source"

