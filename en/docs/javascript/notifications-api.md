---
title: "Notifications API"
slug: "notifications-api"
draft: false
images: []
weight: 9895
type: docs
toc: true
---

## Syntax
- Notification.requestPermission(*callback*)
- Notification.requestPermission().then(*callback*, *rejectFunc*)
- new Notification(*title*, *options*)
- *notification*.close()


The Notifications API was designed to allow browser access to notifying the client.

[Support by browsers][1] might be limited. Also support by the operating system may be limited.

The following table gives an overview of the earliest browser versions that provide support for notifications.

|Chrome|Edge|Firefox|Internet Explorer|Opera|Opera Mini|Safari|
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| 29 | 14 |  46 | no support | 38 | no support | 9.1 |


  [1]: http://caniuse.com/#feat=notifications

## Requesting Permission to send notifications
We use `Notification.requestPermission` to ask the user if he/she wants to receive notifications from our website.

    Notification.requestPermission(function() {
        if (Notification.permission === 'granted') {
            // user approved.
            // use of new Notification(...) syntax will now be successful
        } else if (Notification.permission === 'denied') {
            // user denied.
        } else { // Notification.permission === 'default'
            // user didn’t make a decision.
            // You can’t send notifications until they grant permission.
        }
    });

Since Firefox 47
The `.requestPermission` method can also return a promise when handling the user's decision for granting permission

````
Notification.requestPermission().then(function(permission) {
    if (!('permission' in Notification)) {
        Notification.permission = permission;
    }
    // you got permission !
    }, function(rejection) {
    // handle rejection here.
    }
);
````

## Sending Notifications
After the user has approved a [request for permission to send notifications](https://www.wikiod.com/javascript/notifications-api#Requesting Permission to send notifications), we can send a simple notification that says Hello to the user:

    new Notification('Hello', { body: 'Hello, world!', icon: 'url to an .ico image' });

This will send a notification like this:

> ### Hello
> Hello, world!


## Closing a notification
You can close a notification by using the `.close()` method.

````
let notification = new Notification(title, options);
// do some work, then close the notification
notification.close()
````

You can utilize the `setTimeout` function to auto-close the notification sometime in the future.

````
let notification = new Notification(title, options);
setTimeout(() => {
    notification.close()
}, 4000);
````
The above code will spawn a notification and close it after 4 seconds.


## Notification events
The Notification API specifications support 2 events that can be fired by a Notification.

1. The `click` event.

This event will run when you click on the notification body (excluding the closing X and the Notifications configuration button).

Example:

````
notification.onclick = function(event) {
    console.debug("you click me and this is my event object: ", event);
}
````

2. The `error` event

The notification will fire this event whenever something wrong will happen, like being unable to display

````
notification.onerror = function(event) {
    console.debug("There was an error: ", event);
}
````

