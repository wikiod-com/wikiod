---
title: "Android CI Configuration"
slug: "android-ci-configuration"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Build Tools 24.0.0 - Android
    image: jangrewe/gitlab-ci-android
    before_script:
      - apt-get --quiet update --yes
      - apt-get --quiet install --yes wget tar unzip lib32stdc++6 lib32z1 openjdk-8-jdk
      - echo y | ${ANDROID_HOME}/tools/android --silent update sdk --no-ui --all --filter android-24
      - echo y | ${ANDROID_HOME}/tools/android --silent update sdk --no-ui --all --filter platform-tools
      - echo y | ${ANDROID_HOME}/tools/android --silent update sdk --no-ui --all --filter build-tools-24.0.0
      - echo y | ${ANDROID_HOME}/tools/android --silent update sdk --no-ui --all --filter extra-android-m2repository
      - echo y | ${ANDROID_HOME}/tools/android --silent update sdk --no-ui --all --filter extra-google-google_play_services
      - echo y | ${ANDROID_HOME}/tools/android --silent update sdk --no-ui --all --filter extra-google-m2repository
      - chmod +x gradlew
    build:
      script:
        - ./gradlew assembleDebug
      artifacts:
        paths:
        - app/build/outputs/apk/app-debug.apk
        
Change the build tools number to your compile target, and fork the docker image if you don't want to install everything every time.

