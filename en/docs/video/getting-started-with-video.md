---
title: "Getting started with video"
slug: "getting-started-with-video"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Understanding stand-alone media files
The sample content used here is [Tears of Steel](mango.blender.org), by Blender Foundation. Specifically, we will use the download titled "HD 720p (~365MB, mov, 2.0)". This is a single file that ends with the extension "mov" and will play in just about any modern media player.

Note that the download page offers subtitles as separate SRT file downloads. In this sample content, there are no subtitles delivered together in the same file. We therefore leave subtitle analysis out of scope of this example.

An easy way to analyze various media files is using the tool/library [MediaInfo](https://mediaarea.net). While the analysis functionality showcased here uses the GUI for simplicity, all the features are also available via the MediaInfo API.

By opening this file in the MediaInfo GUI and switching to the tree view, you will see three sections: General, Video and Audio. The first contains basic information about the file, whereas the remaining two each describe a media track found in this file. Let's examine the most relevant information in each section of the output.

# General

[![enter image description here][1]][1]

The first parameters of interest are *Format* and *Format profile*. The first indicates that the **packaging format** is from the MPEG-4 standards suite. MPEG-4 defines the *ISO Base Media File Format* and the *MP4 packaging format*. Furthermore, Apple has created their own specification that derives from these, named in MediaInfo as the "QuickTime" profile.

> Note: Be careful not to confuse MP4 and MPEG-4 - the former refers to a specific packaging format in the MPEG-4 suite of international standards, which also includes video and audio codecs. This can lead to confusion, so avoid using the term MPEG-4 when referring to anything other than the full set of standards.

All packaging formats based on the ISO Base Media File Format, defined in the MPEG-4 standards family, are very similar and can often be processed by the same tools, with their differences being largely a matter of custom vendor extensions that can often be safely ignored. Thus, we can expect the sample video here to be highly compatible with all modern video players.

# Video

[![enter image description here][2]][2]

The most crucial detail about the video track is the codec used to transform raw color data into a compressed form. The name of the codec is provided by the *Format* parameter.

AVC is also known as H.264 and it is the video codec that today is the most widespread, supported on practically all modern devices and software platforms. A video track encoded using AVC is sure to play on just about any player.

Codecs often have multiple *profiles* that allow codec functionality to be divided into tiers, enabling evolution of the technology in a controlled fashion. The *Format profile* parameter indicates that this video uses the Main profile. This profile is relatively uncommon, as just about all modern devices support the High profile, which offers greater compression efficiency.

The quality of the video track is often of paramount importance. Here we see the critical factors expressed by the *Bit rate*, *Width* and *Height* parameters. The latter two hint that this is meant to be a 720p video track, which is considered a lower-end HD quality. The picture is actually shorter vertically than the standard 720p frame of 1280x720 pixels.

The bit rate measures the amount of data that the compressed form of the video stream occupies, on average, for every second of playback. This is a crucial parameter for optimization, as the amount of delivered data is a major source of cost in large-scale video solutions.

The above data points about video quality are simply facts we obtain from analysis - any judgements on the appropriateness of these parameters is a topic that would need far more analysis and is tackled by separate topics in this documentation category, as are many other fine points of working with video tracks.

# Audio

[![enter image description here][3]][3]

Once again, knowing the codec used to encode the audio data is of critical importance. This is expressed by the *Format* and *Format profile* parameters. "MPEG Audio Layer 3" is more commonly known as MP3 and it is a universally supported audio format that can be expected to play everywhere.

As with video, audio quality parameters are the second most important data points, expressed primarily by the *Bit rate* parameter.

# Summary of analysis

The content is packaged using a very popular packaging format, built upon the MPEG-4 standards suite. It is encoded using universally adopted video and audio codecs. From this it is clear that the video is meant to be easily accessible to every viewer - compatibility and availability were key for its authors.

The use of MP3 shows the age of the example content, as it is no longer considered up to par with modern competitors - instead, AAC (Advanced Audio Coding) is the breadwinner in the field of audio codecs.

The same can be said about the use of the H.264 Main profile. It is very rare that any H.264 profile besides High is used, given that almost all decoders support it, enabling everyone to take advantage of the improved efficiency enabled by High profile features.

The bitrates used are slightly higher than expected for today's environment. This may be explained by the authors' desire for high quality or simply by the limitations of the encoders that were available when the content was created.

# More

Other useful tools for media file analysis are [FFprobe](https://ffmpeg.org/), which is part of the FFmpeg software package, and the [Bento4 tools](https://www.bento4.com/) for working with MP4 files. Both are also available in library form. They are capable of more low-level analysis than MediaInfo, in situations where you need to examine individual elements that make up media files.


  [1]: http://i.stack.imgur.com/2YeI5.png
  [2]: http://i.stack.imgur.com/7AxnK.png
  [3]: http://i.stack.imgur.com/NfkxI.png

