---
title: "TensorFlow"
slug: "tensorflow"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

TensorFlow was designed with mobile and embedded platforms in mind. We have sample code and build support you can try now for these platforms:

Android
iOS
Raspberry Pi




Appreciated work by [MindRocks][1]


  [1]: https://blog.mindorks.com/android-tensorflow-machine-learning-example-ff0e9b2654cc

## How to use
Install Bazel from [here][1]. Bazel is the primary build system for TensorFlow.
Now, edit the WORKSPACE, we can find the WORKSPACE file in the root directory of the TensorFlow that we have cloned earlier.

    # Uncomment and update the paths in these entries to build the Android demo.
    #android_sdk_repository(
    #    name = "androidsdk",
    #    api_level = 23,
    #    build_tools_version = "25.0.1",
    #    # Replace with path to Android SDK on your system
    #    path = "<PATH_TO_SDK>",
    #)
    #
    #android_ndk_repository(
    #    name="androidndk",
    #    path="<PATH_TO_NDK>",
    #    api_level=14)

Like below with our sdk and ndk path:

    android_sdk_repository(
        name = "androidsdk",
        api_level = 23,
        build_tools_version = "25.0.1",
        # Replace with path to Android SDK on your system
        path = "/Users/amitshekhar/Library/Android/sdk/",
    )
    android_ndk_repository(
        name="androidndk",
        path="/Users/amitshekhar/Downloads/android-ndk-r13/",
        api_level=14)


  [1]: https://bazel.build/versions/master/docs/install.html

