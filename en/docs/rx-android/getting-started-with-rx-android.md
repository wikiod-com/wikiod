---
title: "Getting started with rx-android"
slug: "getting-started-with-rx-android"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
Add the `rx-android` dependency as well as a current version of `rx-java` to the *build.gradle*.

> Because *RxAndroid* releases are few and far between, it is recommended you also explicitly depend on *RxJava*'s latest version for bug fixes and new features.

### Rx1

    compile 'io.reactivex:rxandroid:1.2.1'
    compile 'io.reactivex:rxjava:1.3.0'

### Rx2

    compile 'io.reactivex.rxjava2:rxandroid:2.0.1'
    compile 'io.reactivex.rxjava2:rxjava:2.1.0'

The use any Android `Scheduler`, create it with `AndroidSchedulers.mainThread()` or `AndroidSchedulers.from(anyLooper)` directly and without any further configuration.

