---
title: "Build QtWebEngine from source"
slug: "build-qtwebengine-from-source"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

Sometimes we need to build QtWebEngine from source for some reason, such as for mp3 support.

## Build on Windows
**Requirements**
* Windows 10, please **set your system locale to English**, otherwise there may be errors
* Visual Studio 2013 or 2015
* QtWebEngine 5.7 source code (could be downloaded from [here](http://download.qt.io/archive/qt/5.7/))
* Qt 5.7 install version, install it and add `qmake.exe` folder to system path
* Python 2, add `python.exe` folder to system path
* Git, add `git.exe` folder to system path
* gperf, add `gperf.exe` folder to system path
* flex-bison, add `win_bison.exe` folder to system path, and rename it to `bison.exe`
>**Note:** I didn't test for Visual Studio versions, all Qt versions.. Let's just take an example here, other versions should be about the same.

**Steps to build**
1. Decompress source code to a folder, let's call it `ROOT`
2. Open `Developer Command Prompt for VS2013`, and go to `ROOT` folder
3. Run `qmake WEBENGINE_CONFIG+=use_proprietary_codecs qtwebengine.pro`. We add this flag to enable mp3 support.
4. Run `nmake`
>**Note:** Mp3 is not supported by QtWebEngine by default, due to license issue. Please make sure to get a license for the codec you added.

