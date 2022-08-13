---
title: "Getting started with ffmpeg"
slug: "getting-started-with-ffmpeg"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
FFmpeg can be installed on a mixture of Operating Systems, including Unix and OS X. Using a command line extension, this utility can also be installed on Windows using a dll. 
<br />
<br />
<h2>OS X</h2>
To install this utility on OS X, just head over to [ffmpeg.org](https://www.ffmpeg.org/download.html#build-mac), download the release relative to your Macs architecture (instructions on finding this can be found [here](https://www.chiefarchitect.com/support/article/KB-01230/determining-if-your-computer-is-32-bit-or-64-bit.html)). Then put the application into an accessible directory and run it from command line.

Another way is using HomeBrew:
https://www.howtogeek.com/211541/homebrew-for-os-x-easily-installs-desktop-apps-and-terminal-utilities/

For example

    brew install ffmpeg --with-fdk-aac --with-ffplay --with-libass --with-libvorbis --with-libvpx --with-rtmpdump --with-openh264 --with-tools

<br />
<br />
<h2>Windows</h2>
To install this utility on Windows, head over to [ffmpeg.org](https://www.ffmpeg.org/download.html#build-windows) and follow the download link, using your architecture. Instructions on finding this can be seen [here](http://answers.microsoft.com/en-us/windows/forum/windows_7-hardware/i-need-to-know-how-to-determine-my-processors/3ede9c69-25f5-427b-8e8d-e9dd2d032d22). Then place the downloaded software into an accessible directory and run from command line.
<br />
<br />
<h2>Unix</h2>
To install this utility on Unix, just follow the instructions found at [ffmpeg.org](https://www.ffmpeg.org/download.html#build-linux)
<br />
<br />
To check if ffmpeg is installed correctly and see a list of available commands try running the following command in the command line:

    ffmpeg -help

## What is FFmpeg?
[FFmpeg][1] (or "Fast Forward MPEG") is a simple yet feature rich command line utility to allow the manipulation (including decoding, encoding, transcoding, muxing, demuxing, streaming, filtering, and playing) of media and video files. This utility differs from other GUI orientated software as it employs the WYSIWYG methodology (What You See Is What You Get). Instead of hidden away menus and features, everything can be found by just typing `ffmpeg -h` when set up, or following the [comprehensive documentation][2]. In addition to the command line tool there are a number of C libraries (which it uses), that can be used to bring the functionality of FFmpeg into other projects. The documentation for the libraries includes many [examples][3] to help get you started.


  [1]: https://www.ffmpeg.org/about.html
  [2]: https://www.ffmpeg.org/documentation.html
  [3]: https://www.ffmpeg.org/doxygen/3.1/examples.html

