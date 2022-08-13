---
title: "Extension for rich Push Notification - iOS 10."
slug: "extension-for-rich-push-notification---ios-10"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

iOS 10 gave us `UserNotifications.framework`, the new API for local/remote notifications.
It offers viewing media attachments or responding to messages right from the notification.

Notification content consists of: title, subtitle, body and attachment. 
Attachment can contain images/gifs/videos up to 50 mb.

## Notification Content Extension
**Why do we need it?** 

Content extension helps us to create custom user interface upon notification expanasion.

You use this framework to define an extension that receives the notification data and provides the corresponding visual representation. Your extension can also respond to custom actions associated with those notifications.



## Implementation
1. In xCode `Navigator` window go to `Targets` section. Press `Add New Target`.
2. Select `Notification Content Extension` template:

[![enter image description here][1]][1]


3. In your `info.plist` file set the identifier for `UNNotificationExtensionCategory` key:

[![enter image description here][2]][2]

*NSExtensionAttributes*:

`UNNotificationExtensionCategory` (Required) 

The value of this key is a string or an array of strings. Each string contains the identifier of a category declared by the app using the UNNotification​Category class.

`UNNotificationExtensionInitialContentSizeRatio` (Required)

 Number that represents the initial size of your view controller’s view expressed as a ratio of its height to its width.

`UNNotificationExtensionDefaultContentHidden` (Optional)

When set to YES, the system displays only your custom view controller in the notification interface. When set to NO, the system displays the default notification content in addition to your view controller’s content.

`UNNotificationExtensionOverridesDefaultTitle` (Optional) 

The value of this key is a Boolean. When set to true, the system uses the title property of your view controller as the title of the notification. When set to false, the system sets the notification's title to the name of your app. If you do not specify this key, the default value is set to false.


4. Create custom view in `NotificationViewController.swift` file
5. Add new `category key` and set its value to what we typed in the Info.plist (step 3):

Push:

    {
     aps: {
     alert: { … },
     category: 'io.swifting.notification-category' 
     }
    }

Local: 

    let mutableNotificationContent = UNMutableNotificationContent()
    mutableNotificationContent.category = "io.swifting.notification-category"
    mutableNotificationContent.title = "Swifting.io Notifications"
    mutableNotificationContent.subtitle = "Swifting.io presents"
    mutableNotificationContent.body = "Custom notifications"

*Also check out the official API reference:* https://developer.apple.com/reference/usernotificationsui/unnotificationcontentextension?utm_source=swifting.io&utm_medium=web&utm_campaign=blog%20post

  [1]: https://i.stack.imgur.com/5EUot.jpg
  [2]: https://i.stack.imgur.com/nI8ck.jpg

