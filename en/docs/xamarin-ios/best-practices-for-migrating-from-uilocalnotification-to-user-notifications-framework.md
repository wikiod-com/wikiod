---
title: "Best practices for migrating from UILocalNotification to User Notifications framework"
slug: "best-practices-for-migrating-from-uilocalnotification-to-user-notifications-framework"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## UserNotifications
1. You will have to import UserNotifications

        @import UserNotifications;

2.Request authorization for localNotification

    let center = UNUserNotificationCenter.current()
    center.requestAuthorization([.alert, .sound]) { (granted, error) in
        // Enable or disable features based on authorization.
    }

3. Now we will update the application icon badge number

        @IBAction  func triggerNotification(){
        let content = UNMutableNotificationContent()
        content.title = NSString.localizedUserNotificationString(forKey: "Tom said:", arguments: nil)
        content.body = NSString.localizedUserNotificationString(forKey: "Hello Mike！Let's go.", arguments: nil)
        content.sound = UNNotificationSound.default()
        content.badge = UIApplication.shared().applicationIconBadgeNumber + 1;
        content.categoryIdentifier = "com.mike.localNotification"
        //Deliver the notification in two seconds.
        let trigger = UNTimeIntervalNotificationTrigger.init(timeInterval: 1.0, repeats: true)
        let request = UNNotificationRequest.init(identifier: "TwoSecond", content: content, trigger: trigger)

        //Schedule the notification.
        let center = UNUserNotificationCenter.current()
        center.add(request)
        }

        @IBAction func stopNotification(_ sender: AnyObject) {
        let center = UNUserNotificationCenter.current()
        center.removeAllPendingNotificationRequests()
        }

