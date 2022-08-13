---
title: "Platform Module"
slug: "platform-module"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Find the OS Type/Version
The first step is to import Platform from the 'react-native' package like so:

<!-- language: lang-js -->

    import { Platform } from 'react-native'

After you've done that, you can go ahead and access the OS type through `Platform.OS` allowing you to use it in conditional statements like 

<!-- language: lang-js -->

    const styles = StyleSheet.create({
      height: (Platform.OS === 'ios') ? 200 : 100,
    })

If you want to detect the Android version, you can use `Platform.Version` like so:

<!-- language: lang-js -->

    if (Platform.Version === 21) {
      console.log('Running on Lollipop!');
    }

For iOS, Platform.Version is returning a String, for complex condition don't forget to parse it.

<!-- language: lang-js -->

    if (parseInt(Platform.Version, 10) >= 9) {
        console.log('Running version higher than 8');
    }

If the platform specific logic is complex, one can render two different files based on platform. 
Ex:

- `MyTask.android.js`
- `MyTask.ios.js`

and require it using 

<!-- language: lang-js -->

    const MyTask = require('./MyTask')

