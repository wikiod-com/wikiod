---
title: "Decoding"
slug: "decoding"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Getting the raw video/audio from encoded media streams.

## Decode frames
Given a codec context and encoded packets from a media stream, you can start decoding media into raw frames. To decode a single frame, you can use the following code:

```cpp
// A codec context, and some encoded data packet from a stream/file, given.
AVCodecContext *codecContext;  // See Open a codec context
AVPacket *packet;              // See the Reading Media topic


// Send the data packet to the decoder
int sendPacketResult = avcodec_send_packet(codecContext, packet);
if (sendPacketResult == AVERROR(EAGAIN)){
    // Decoder can't take packets right now. Make sure you are draining it.
}else if (sendPacketResult < 0){
    // Failed to send the packet to the decoder
}

// Get decoded frame from decoder
AVFrame *frame = av_frame_alloc();
int decodeFrame = avcodec_receive_frame(codecContext, frame);

if (decodeFrame == AVERROR(EAGAIN)){
    // The decoder doesn't have enough data to produce a frame
    // Not an error unless we reached the end of the stream
    // Just pass more packets until it has enough to produce a frame
    av_frame_unref(frame);
    av_freep(frame);
}else if (decodeFrame < 0){
    // Failed to get a frame from the decoder
    av_frame_unref(frame);
    av_freep(frame);
}

// Use the frame (ie. display it)
```

If you want to decode *all* frames, you can simply place the previous code in a loop, feeding it consecutive packets.


## Find a stream
Media stream containers usually have a several streams, such as a video stream and an audio stream. For example, you can get the audio stream using the following:

```cpp
// A Format Context - see Reading Data for more info
AVFormatContext *formatContext;

// Inspect packets of stream to determine properties
if (avformat_find_stream_info(formatContext, NULL) < 0){
    // Error finding info
}

// Find the stream and its codec
AVCodec* audioCodec;
int audioStreamIndex = av_find_best_stream(
    formatContext,        // The media stream
    AVMEDIA_TYPE_AUDIO,   // The type of stream we are looking for - audio for example
    -1,                   // Desired stream number, -1 for any
    -1,                   // Number of related stream, -1 for none
    &audioCodec,          // Gets the codec associated with the stream, can be NULL
    0                     // Flags - not used currently
);
if(audioStreamIndex = AVERROR_STREAM_NOT_FOUND || !audioCodec){
    // Error finding audio (ie. no audio stream?)
}
```

To get other types of streams, you just need to replace the type of stream. The following are valid types:

```
AVMEDIA_TYPE_VIDEO,
AVMEDIA_TYPE_AUDIO,
AVMEDIA_TYPE_SUBTITLE,
AVMEDIA_TYPE_DATA,        // Usually continuous
AVMEDIA_TYPE_ATTACHMENT,  // Usually sparse
```

## Open a codec context
Once you have a stream Format Context and its respective Codec, you can open it for decoding using the following code:

```cpp
// The format context and codec, given - see Find a stream for how to get these
AVFormatContext *formatContext;
AVCodec* codec;
int streamIndex;

// Get the codec context
AVCodecContext *codecContext = avcodec_alloc_context3(codec);
if (!codecContext){
    // Out of memory
    avformat_close_input(&formatContext);
}

// Set the parameters of the codec context from the stream
int result = avcodec_parameters_to_context(
    codecContext,
    formatContext->streams[streamIndex]->codecpar
);
if(result < 0){
    // Failed to set parameters
    avformat_close_input(&formatContext);
    avcodec_free_context(&codecContext);
}

// Ready to open stream based on previous parameters
// Third parameter (NULL) is optional dictionary settings
if (avcodec_open2(codecContext, codec, NULL) < 0){
    // Cannot open the video codec
    codecContext = nullptr;
}

// Do something with the opened codec context... (ie decode frames through the context)
```

