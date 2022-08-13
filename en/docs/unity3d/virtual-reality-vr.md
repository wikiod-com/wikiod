---
title: "Virtual Reality (VR)"
slug: "virtual-reality-vr"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Enabling VR support
In Unity Editor, open **Player Settings** (Edit > Project Settings > Player).

Under **Other Settings**, check *Virtual Reality Supported*.

[![enter image description here][1]][1]

Add or remove VR devices for each build target in the *Virtual Reality SDKs* list under the checkbox.


  [1]: http://i.stack.imgur.com/6CnC4.png

## VR Platforms
There are two main platforms in VR, one is mobile platform, like **Google Cardboard**, **Samsung GearVR**, the other is PC platform, like **HTC Vive, Oculus, PS VR**...

Unity officially supports the **Oculus Rift**, **Google Carboard**, **Steam VR**, **Playstation VR**, **Gear VR**, and the **Microsoft Hololens**.

Most platforms have their own support and sdk. Usually, you need to download the sdk as an extension firstly for unity.

## SDKs:
- [Google Cardboard](https://github.com/googlevr/gvr-unity-sdk)
- [Daydream Platform](https://developers.google.com/vr/unity/download)
- [Samsung GearVR](http://www.gearvrf.org/bin/view/GearVRF/GearVRfWikiGetStarted#GearDocuments) (integrated since Unity 5.3)
- [Oculus Rift](https://developer.oculus.com/downloads/)
- [HTC Vive/Open VR](https://github.com/ValveSoftware/openvr)
- [Microsoft Hololens](https://unity3d.com/partners/windows/hololens#download)

## Documentation:
- [Google Cardboard/Daydream](https://developers.google.com/vr/android/)
- [Samsung GearVR](http://www.gearvrf.org/bin/view/GearVRF/GearVRfWikiGetStarted#GearDocuments)
- [Oculus Rift](https://developer3.oculus.com/documentation/)
- [HTC Vive](https://github.com/ValveSoftware/openvr/wiki/API-Documentation)
- [Microsoft Hololens](https://developer.microsoft.com/en-us/windows/holographic/documentation)

## Hardware
There is a necessary hardware dependency for a VR application, that usually depends on the platform that you're building for. There are 2 broad categories for hardware devices based on their motion capabilities:
1. 3 DOF (Degrees of Freedom)
2. 6 DOF (Degrees of Freedom)

3 DOF means that the motion of the Head-Mounted Display (HMD) is constrained to operate in 3 dimensions that is rotate about the three orthogonal axes centered on the HMDs center of gravity - the longitudinal, vertical and horizontal axes. Motion about the longitudinal axis is called roll, motion about the lateral axis is called pitch and motion about the perpendicular axis is called yaw, similar principles that govern motion of any moving object like an aeroplane or a car, which means that although you will be able to see in all X, Y, Z directions by the motion of your HMD in the Virtual environment, but you wouldn’t be able to move or touch anything (motion by an additional bluetooth controller is not the same).

However, 6 DOF allows for a room-scale experience wherein you can also move about the X,Y  and Z axis apart from the roll, pitch and yaw motions about its centre of gravity, hence the 6 degree of freedom.

Currently a Room-scale VR facilitated for 6 DOF requires high computation performance with a high-end graphic card and RAM that you probably won’t get from your standard laptops and will require a desktop computer with optimal performance and also at least 6ft × 6ft free space, whereas a 3 DOF experience can be achieved by just a standard smart phone with an inbuilt gyro (which is inbuilt in most modern smart phones that cost about $200 or more).

Some common devices available in the market today are:

 - [Oculus Rift][1] (6 DOF)
 - [HTC Vive][2] (6 DOF)
 - [Daydream][3] (3 DOF)
 - [Gear VR Powered by Oculus][4] (3 DOF)
 - [Google Cardboard][5] (3 DOF)


  [1]: https://www3.oculus.com/en-us/rift/
  [2]: https://www.vive.com/us/
  [3]: https://vr.google.com/daydream/
  [4]: https://www3.oculus.com/en-us/gear-vr/
  [5]: https://vr.google.com/cardboard/

