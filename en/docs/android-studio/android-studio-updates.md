---
title: "Android Studio updates"
slug: "android-studio-updates"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Updating Android Studio in Ubuntu
If you are able to download an update of Android Studio, but after it restarts nothing happens, check out the following example:

1. After the patch was downloaded and Android Studio closed, open the terminal
2. Go to your android studio folder, e.g.
`cd ~/android-studio `
3. Go to `bin` subfolder: 
`cd bin`
4. Make sure your `studio.sh` file has run permissions: 
`chmod +x studio.sh`
5. Run Android Studio from here: `./studio.sh`

After that Android Studio will find the patch and install it. Then you may close Android Studio and run it the regular way (whatever way you prefer, I run it from Launcher pad). 

## Android Studio update channels
Overview
-----

Android Studio's built-in update mechanism can be set to receive updates through any one of these **four** channels: 
 - **Canary:** Bleeding edge, released about weekly. These are early previews released in order to obtain real-world feedback during development. The canary channel will always have the latest build, including updates to beta or stable releases. We recommend running canary builds side-by-side with a beta or stable installation.
- **Dev:** Canary builds after a full round of internal testing get promoted into the Dev Channel.
 - **Beta:** Release candidates based on stable canary builds, released and updated to obtain feedback prior to the stable release. The beta channel will be updated with new stable builds until a new canary build goes to beta. 
 - **Stable:** The official stable release, as available from the Android Developer site.
 
Download the full installations of the build offered in each of these channels: [Canary](http://tools.android.com/download/studio/canary), [Dev](http://tools.android.com/download/studio/dev), [Beta](http://tools.android.com/download/studio/beta), [Stable](http://tools.android.com/download/studio/stable).

Alternatively, you can build it yourself, following the instructions in [Build Overview](http://tools.android.com/build). 

Selecting an Update Channel
------

To select the update channel for an Android Studio installation go through:

 `File > Settings > System Settings > Updates` 

 and choose the appropriate channel on which to check for updates:

[![enter image description here][1]][1]

When an update is available, you'll be prompted by the IDE:
[![enter image description here][2]][2]

Selecting update will display the update information dialog with details information on the patch available, its size and its channel:

[![enter image description here][3]][3]


  [1]:http://tools.android.com/_/rsrc/1461863556747/download/studio/UpdateChannelSettings.png?height=175&width=400
  [2]: http://i.stack.imgur.com/svk07.png?height=179&width=400
  [3]: http://tools.android.com/_/rsrc/1461863594004/download/studio/NewUpdateDetail.png?height=179&width=400

