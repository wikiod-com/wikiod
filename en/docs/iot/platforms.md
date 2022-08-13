---
title: "Platforms"
slug: "platforms"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

In simple words the purpose of any IoT device is to connect with other IoT devices and applications (cloud-based mostly) to relay information using internet transfer protocols.

The gap between the device sensors and data networks is filled by an IoT Platform.

## Kaa
[Kaa](https://www.kaaproject.org/) is a good example of OpenSource IoT Platforms, it provides a data collection, notification and device to device communication for you.

| Language | Connectivity Protocols                    |
| -------- | ----------------------------------------- |
| Java     | Kaa Protocol (KP) over MQTT, CoAP and TCP |

Kaa device to device communication is based on Events and you can send events even into server instead of devices. as its documentation says:
> The Kaa Events subsystem is designed to generate endpoint events in real time, send them to other endpoints of the same owner and to Kaa server for processing.


