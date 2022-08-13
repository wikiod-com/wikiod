---
title: "Hardware Button EventsIntents (PTT, LWP, etc.)"
slug: "hardware-button-eventsintents-ptt-lwp-etc"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Several android devices have custom buttons added by the manufacturer. This opens new possibilities for the developer in handling those buttons especially when making Apps targeted for Hardware Devices.

This topic documents buttons which have intents attached to them which you can listen for via intent-receivers.

## Sonim Devices
Sonim devices have varying by model a lot of different custom buttons:

# PTT_KEY

    com.sonim.intent.action.PTT_KEY_DOWN
    com.sonim.intent.action.PTT_KEY_UP

# YELLOW_KEY

    com.sonim.intent.action.YELLOW_KEY_DOWN
    com.sonim.intent.action.YELLOW_KEY_UP

# SOS_KEY

    com.sonim.intent.action.SOS_KEY_DOWN
    com.sonim.intent.action.SOS_KEY_UP

# GREEN_KEY

    com.sonim.intent.action.GREEN_KEY_DOWN
    com.sonim.intent.action.GREEN_KEY_UP

# Registering the buttons
To receive those intents you will have to assign the buttons to your app in the Phone-Settings.
Sonim has a possibilty to auto-register the buttons to the App when it is installed. In order to do that you will have to contact them and get a package-specific key to include in your Manifest like this:

    <meta-data
        android:name="app_key_green_data"
        android:value="your-key-here" />

## RugGear Devices
# PTT Button

    android.intent.action.PTT.down
    android.intent.action.PTT.up

Confirmed on: RG730, RG740A

