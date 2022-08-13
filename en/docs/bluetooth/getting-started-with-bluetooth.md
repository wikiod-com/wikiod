---
title: "Getting started with bluetooth"
slug: "getting-started-with-bluetooth"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Profiles
The Bluetooth specification contains several profile specifications. A profile describes how to use and implement a function.

They can depend on each other, here is a basic layout of the most common profile dependencies

[![enter image description here][1]][1]


All profiles can be found at [BT SIG][2], be aware that different versions might contain different functionality. Also note that some of the profiles contains several categories, these are sometimes optional, so make sure that your device supports the category in question.
Here are some of the most common *smartphone Profiles* and their specifications

**A2DP** - Advanced Audio Distribution Profile<br>
The Advanced Audio Distribution Profile (A2DP) defines the protocols and procedures that realize distribution of audio content of high-quality in mono or stereo on ACL channels.
A typical usage case is the streaming of music content from a stereo music player to headphones or speakers. The audio data is compressed in a proper format for efficient use of the limited bandwidth.<br>
Dependencies: GAVDP, GAP

**AVRCP** - Audio/Video Remote Control Profile<br>
The Audio/Video Remote Control Profile (AVRCP) defines the features and procedures required in order to ensure interoperability between Bluetooth devices with audio/video control functions in the Audio/Video distribution scenarios.
This profile adopts the AV/C device model and command format for control messages, and those messages are transported by the Audio/Video Control Transport Protocol (AVCTP).
In this profile, the controller translates the detected user action to the A/V control signal, and then transmits it to a remote Bluetooth device. In addition to this the profile uses Bluetooth specific extensions to support transfer of metadata related to content to be transferred between Bluetooth devices. The remote control described in this profile is designed specific to A/V control.<br>
Dependencies: GAP

**HFP** - Hands-Free Profile<br>
This document defines the protocols and procedures that shall be used by devices implementing the Hands-Free Profile. The most common examples of such devices are in-car Hands-Free units used together with cellular phones, or wearable wireless headsets. The profile defines how two devices supporting the Hands-Free Profile shall interact with each other on a point-to-point basis. An implementation of the Hands-Free Profile typically enables a headset, or an embedded hands-free unit to connect, wirelessly, to a cellular phone for the purposes of acting as the cellular phone’s audio input and output mechanism and allowing typical telephony functions to be performed without access to the actual phone.<br>
Dependencies: SPP, GAP

**HSP** - Headset Profile<br>
This Headset profile defines the protocols and procedures that shall be used by devices requiring a full-duplex audio connection combined with minimal device control commands. The most common examples of such devices are headsets, personal computers, PDAs, and cellular phones, though most cellular phones will prefer to use a more advanced profile such as Hands-Free Profile.
The headset can be wirelessly connected for the purposes of acting as the device’s audio input and output mechanism, providing full duplex audio.<br>
Dependencies: SPP, GAP

**PBAP** - Phonebook Access Profile<br>
The Phone Book Access Profile (PBAP) defines the protocols and procedures that shall be used by devices for the retrieval of phone book objects. It is based on a Client-Server interaction model where the Client device pulls phone book objects from the Server device.
This profile is especially tailored for the Hands-Free usage case (i.e. implemented in combination with the “Hands-Free Profile” or the “SIM Access Profile”). It provides numerous capabilities that allow for advanced handling of phone book objects, as needed in the car environment. In particular, it is much richer than the Object Push Profile ( that could be used to push vCard formatted phone book entry from one device to another).
This profile can also be applied to other usage cases where a Client device is to pull phone book objects from a Server device. Note however that this profile only allows for the consultation of phone book objects (read-only). It is not possible to alter the content of the original phone book object (read/write).<br>
Dependencies: GOEP, SPP, GAP


  [1]: http://i.stack.imgur.com/ihq8F.png
  [2]: https://www.bluetooth.com/specifications/adopted-specifications

## Installation or Setup
Detailed instructions on getting bluetooth set up or installed.

