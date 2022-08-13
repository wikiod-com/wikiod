---
title: "Publish-Subscribe"
slug: "publish-subscribe"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Publish-Subscribe in Java


## Simple pub-sub example in JavaScript
Publishers and subscribers don't need to know each other. They simply communicate with the help of message queues.

    (function () {
            var data;

            setTimeout(function () {
                data = 10;
                $(document).trigger("myCustomEvent");
            }, 2000);

            $(document).on("myCustomEvent", function () {
                console.log(data);
            });
    })();
Here we published a custom event named **myCustomEvent** and subscribed on that event. So they don't need to know each other.  

