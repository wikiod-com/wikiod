---
title: "Transferring files using push and pull"
slug: "transferring-files-using-push-and-pull"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
 - adb push [-p] LOCAL REMOTE
 - adb pull [-a] [-p] REMOTE [LOCAL]

## Parameters
|Parameters|Details|
|---|---|
|LOCAL|A file or directory that is located on the user's computer|
|REMOTE|A file or directory that is located on the user's Android device|
|-a|Also copy the file the remote file's timestamp and file mode data
|-p|Display transfer progress while the file or directory is copying

If LOCAL is omitted in the adb pull command, the filename from REMOTE is used

LOCAL can be a relative path or an absolute path, but REMOTE must be an absolute path

## Push a file to the SD card
    adb push file.txt /sdcard/

## Pull a file from the SD card to the current working directory
    adb pull /sdcard/file.txt

