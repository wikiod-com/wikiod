---
title: "Media Elements"
slug: "media-elements"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Parameters
| Attribute | Details |
| ------    | ------ |
| width     | Sets the element's width in pixels.   |
| height    | Sets the element's height in pixels.   |
| `<source>`| Defines resources of the audio or video files   |
| track     | Defines the text track for media elements |
| controls  | Displays controls |
| autoplay  | Automatically start playing the media |
| loop      | Plays the media in a repeated cycle |
| muted     | Plays the media without sound |
| poster    | Assigns an image to display until a video is loaded |


## Support in browsers

|Feature|Chrome|Firefox (Gecko)|Internet Explorer|Opera|Safari|
| ------ | ------ | ------ | ------ | ------ | ------ |
|Basic support|3.0|3.5 (1.9.1)|9.0|10.50|3.1|
|`<audio>`: PCM in WAVE|(Yes)|3.5 (1.9.1)|No support|10.50|3.1|
|`<audio>`: Vorbis in WebM|(Yes)|4.0 (2.0)|No support|10.60|3.1|
|`<audio>`: Streaming Vorbis/Opus in WebM via MSE|?|36.0|?|?|?
|`<audio>`: Vorbis in Ogg|(Yes)|3.5 (1.9.1)|No support|10.50|No support|
|`<audio>`: MP3|(Yes)|(Yes)|9.0|(Yes)|3.1|
|`<audio>`: MP3 in MP4|?|?|?|?|(Yes)|
|`<audio>`: AAC in MP4|(Yes)|(Yes)|9.0|(Yes)|3.1|
|`<audio>`: Opus in Ogg|27.0|15.0 (15.0)|?|?|?|
|`<video>`: VP8 and Vorbis in WebM|6.0|4.0 (2.0)|9.0|10.60|3.1|
|`<video>`: VP9 and Opus in WebM|29.0|28.0 (28.0)|?|(Yes)|?|
|`<video>`: Streaming WebM via MSE|?|42.0 (42.0)|?|?|?|
|`<video>`: Theora and Vorbis in Ogg|(Yes)|3.5 (1.9.1)|No support|10.50|No support|
|`<video>`: H.264 and MP3 in MP4|(Yes)|(Yes)|9.0|(Yes)|(Yes)|
|`<video>`: H.264 and AAC in MP4|(Yes)|(Yes)|9.0|(Yes)|3.1|
|any other format|No support|No support|No support|No support|3.1|


## Using `<video>` and `<audio>` element to display audio/video content


## Audio
HTML5 provides a new standard for embedding an audio file on a web page.

You can embed an audio file to a page using the `<audio>` element:

    <audio controls>
      <source src="file.mp3" type="audio/mpeg">
    Your browser does not support the audio element.
    </audio>

## Video
You can embed also a video to a webpage using the `<video>` element:

    <video width="500" height="700" controls>
      <source src="video.mp4" type="video/mp4">
    Your browser does not support the video tag.
    </video>

## Video header or background

Adding a video that will autoplay on a loop and has no controls or sound. Perfect for a video header or background.

    <video width="1280" height="720" autoplay muted loop poster="video.jpg" id="videobg">
      <source src="video.mp4" type="video/mp4">
      <source src="video.webm" type="video/webm">
      <source src="video.ogg" type="video/ogg">
    </video>

This CSS provides a fallback if the video cannot be loaded. Note that is it recomended to use the first frame of the video as the poster video.jpg.

    #videobg {
      background: url(video.jpg) no-repeat;
      background-size: cover;
    }

