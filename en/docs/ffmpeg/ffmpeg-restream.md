---
title: "Ffmpeg Restream"
slug: "ffmpeg-restream"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Simple Device Restream
Ffmpeg is a swiss knife for streaming project. For any kind of device streaming you only need to get the specification of device. To list the device

    ffmpeg -f dshow -list_devices true  -i dummy

Command prompt will list all the aviable device on machine. 

    [dshow @ 0000000004052420] DirectShow video devices
    [dshow @ 0000000004052420]  "ManyCam Virtual Webcam"
    [dshow @ 0000000004052420]  "UScreenCapture"
    [dshow @ 0000000004052420] DirectShow audio devices


For restreaming the audio&video device,

    ffmpeg -f dshow -i video="DirectShow video devices":audio="DirectShow audio devices" 
    -vcodec libx264 -acodec aac -strict experimental 2 -tune zerolatency -f flv 
    rmtp://WOWZA_IP/WOWZA_APP/STREAMNAME

This can be extended all kind of device like medical devices or video hardware.




