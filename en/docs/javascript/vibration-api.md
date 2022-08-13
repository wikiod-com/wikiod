---
title: "Vibration API"
slug: "vibration-api"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Modern mobile devices include hardware for vibrations. The Vibration API offers Web apps the ability to access this hardware, if it exists, and does nothing if the device doesn't support it.

## Syntax
- let success = window.navigator.vibrate( pattern );


[Support by browsers][1] might be limited. Also support by the operating system may be limited.

The following table gives an overview of the earliest browser versions that provide support for vibrations.

|Chrome|Edge|Firefox|Internet Explorer|Opera|Opera Mini|Safari|
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| 30 | *no support* |  16 | *no support* | 17 | *no support* | *no support* |


  [1]: http://caniuse.com/#feat=vibration

## Single vibration
Vibrate the device for 100ms:

    window.navigator.vibrate(100);

or

    window.navigator.vibrate([100]);


## Check for support
Check if browser supports vibrations

    if ('vibrate' in window.navigator)
        // browser has support for vibrations
    else
        // no support

## Vibration patterns
An array of values describes periods of time in which the device is vibrating and not vibrating.

    window.navigator.vibrate([200, 100, 200]);


