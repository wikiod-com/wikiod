---
title: "Reading Media"
slug: "reading-media"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

There are a few ways to read Audio/Video into FFmpeg.

## Reading from memory
libavformat usually takes in a file name and reads media directly from the filesystem. If you want to read from memory (such as streams), do the following:

```cpp
// Define your buffer size
const int FILESTREAMBUFFERSZ = 8192;


// A IStream - you choose where it comes from
IStream* fileStreamData;

// Alloc a buffer for the stream
unsigned char* fileStreamBuffer = (unsigned char*)av_malloc(FILESTREAMBUFFERSZ);
if (fileStreamBuffer == nullptr){
    // out of memory
}

// Get a AVContext stream
AVIOContext* ioContext = avio_alloc_context(
    fileStreamBuffer,    // Buffer
    FILESTREAMBUFFERSZ,  // Buffer size
    0,                   // Buffer is only readable - set to 1 for read/write
    fileStreamData,      // User (your) specified data
    FileStreamRead,      // Function - Reading Packets (see example)
    0,                   // Function - Write Packets
    FileStreamSeek       // Function - Seek to position in stream (see example)
);
if(ioContext == nullptr){
    // out of memory
}

// Allocate a AVContext
AVFormatContext *formatContext = avformat_alloc_context();

// Set up the Format Context
formatContext->pb = ioContext;
formatContext->flags |= AVFMT_FLAG_CUSTOM_IO; // we set up our own IO

// Open "file" (open our custom IO)
// Empty string is where filename would go. Doesn't matter since we aren't reading a file
// NULL params are format and demuxer settings, respectively
if (avformat_open_input(&formatContext, "", nullptr, nullptr) < 0){
    // Error opening file
}

// Do something with the formatContext

// Free resources!
avformat_close_input(&formatContext);
av_free(ioContext);
```

## Reading from a file
Opening a media file from the local file system.

```cpp
AVFormatContext *formatContext;

// Open the file
if(avformat_open_file(&formatContext, "path/to/file.ogg", NULL, NULL) < 0){
    // Error opening file
}

// Do something with the file

// Free resources
avformat_close_input(&formatContext);

```

## Reading from a format context
Formats contain one or more encoded and muxed streams. We usually read these in chunks, which are often called frames (though in certain cases, FFmpeg refers exclusively to decoded, raw media chunks as frames, and encoded chunks as packets, which may be confusing). To read a single frame from a format, use the following:

```cpp
// A Format Context - see other examples on how to create it
AVFormatContext *formatContext;

// Initialize the AVPacket manually
AVPacket avPacket;
av_init_packet(&avPacket); // set fields of avPacket to default.
avPacket.data = NULL;
avPacket.size = 0;

// Read from the context into the packet
if(av_read_frame(formatContext, &avPacket) == 0){
    // nothing read
}

// Use the packet (such as decoding it and playing it)

// Free packet
av_packet_unref(&avPacket);

```

## Reading from an IStream in a IOContext
The API call `avio_alloc_context`, which sets up a custom IO context, takes in a pointer to a Read function. If you are reading from an IStream, you can use the following:

```cpp
/**
 * Reads from an IStream into FFmpeg.
 *
 * @param ptr       A pointer to the user-defined IO data structure.
 * @param buf       A buffer to read into.
 * @param buf_size  The size of the buffer buff.
 *
 * @return The number of bytes read into the buffer.
 */
int FileStreamRead(void* ptr, uint8_t* buf, int buf_size)
{
    // This is your IStream
    IStream* stream = reinterpret_cast<IStream*>(ptr);

    ULONG bytesRead = 0;
    HRESULT hr = stream->Read(buf, buf_size, &bytesRead);
    if(hr == S_FALSE)
        return AVERROR_EOF;  // End of file

    if(FAILED(hr))
        return -1;
    return bytesRead;
}
```

## Seeking within an IStream in an IOContext
The API call `avio_alloc_context`, which sets up a custom IO context, takes in a pointer to a **Seek** function. If you are reading from an IStream, you can use the following:

```cpp
/**
 * Seeks to a given position on an IStream.
 * 
 * @param ptr     A pointer to the user-defined IO data structure.
 * @param pos     The position to seek to.
 * @param origin  The relative point (origin) from which the seek is performed.
 *
 * @return  The new position in the IStream.
 */
int64_t FileStreamSeek(void* ptr, int64_t pos, int origin){
    // Your custom IStream
    IStream* stream = reinterpret_cast<IStream*>(ptr);
 
    // Prevent overflows
    LARGE_INTEGER in = { pos };
    ULARGE_INTEGER out = { 0 };

    // Origin is an item from STREAM_SEEK enum.
    //   STREAM_SEEK_SET - relative to beginning of stream.
    //   STREAM_SEEK_CUR - relative to current position in stream.
    //   STREAM_SEEK_END - relative to end of stream.
    if(FAILED(stream->Seek(in, origin, &out)))
         return -1;
 
    // Return the new position
    return out.QuadPart;
}
```

