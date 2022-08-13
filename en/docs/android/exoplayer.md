---
title: "ExoPlayer"
slug: "exoplayer"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Add ExoPlayer to the project
**Via jCenter**

including the following in your project's build.gradle file:

    compile 'com.google.android.exoplayer:exoplayer:rX.X.X'
where rX.X.X is the your preferred version. For the latest version, see the project's [Releases][1]. For more details, see the project on [Bintray][2].


  [1]: https://github.com/google/ExoPlayer/releases
  [2]: https://bintray.com/google/exoplayer/exoplayer/view

## Using ExoPlayer
Instantiate your ExoPlayer:

    exoPlayer = ExoPlayer.Factory.newInstance(RENDERER_COUNT, minBufferMs, minRebufferMs);

To play audio only you can use these values:

    RENDERER_COUNT = 1 //since you want to render simple audio
    minBufferMs = 1000 
    minRebufferMs = 5000

Both buffer values can be tweaked according to your requirements.

Now you have to create a DataSource. When you want to stream mp3 you can use the DefaultUriDataSource. You have to pass the Context and a UserAgent. To keep it simple play a local file and pass null as userAgent:

    DataSource dataSource = new DefaultUriDataSource(context, null);

Then create the sampleSource:

    ExtractorSampleSource sampleSource = new ExtractorSampleSource(
                    uri, dataSource, new Mp3Extractor(), RENDERER_COUNT, requestedBufferSize);

uri points to your file, as an Extractor you can use a simple default Mp3Extractor if you want to play mp3. requestedBufferSize can be tweaked again according to your requirements. Use 5000 for example.

Now you can create your audio track renderer using the sample source as follows:

    MediaCodecAudioTrackRenderer audioRenderer = new MediaCodecAudioTrackRenderer(sampleSource);

Finally call prepare on your exoPlayer instance:

    exoPlayer.prepare(audioRenderer);

To start playback call:

    exoPlayer.setPlayWhenReady(true);

## Main steps to play video & audio using the standard TrackRenderer implementations
    // 1. Instantiate the player.
    player = ExoPlayer.Factory.newInstance(RENDERER_COUNT);
    // 2. Construct renderers.
    MediaCodecVideoTrackRenderer videoRenderer = ...
    MediaCodecAudioTrackRenderer audioRenderer = ...
    // 3. Inject the renderers through prepare.
    player.prepare(videoRenderer, audioRenderer);
    // 4. Pass the surface to the video renderer.
    player.sendMessage(videoRenderer, MediaCodecVideoTrackRenderer.MSG_SET_SURFACE,     surface);
    // 5. Start playback.
    player.setPlayWhenReady(true);
    ...
    player.release(); // Don’t forget to release when done!

