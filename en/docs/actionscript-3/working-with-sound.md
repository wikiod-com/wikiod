---
title: "Working with Sound"
slug: "working-with-sound"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
* Sound.play(startTime:Number = 0, loops:int = 0, sndTransform:flash.media:SoundTransform = null):SoundChannel // Plays a loaded sound, returns a SoundChannel

## Stop Playing a Sound
    import flash.net.URLRequest;
    import flash.media.Sound;
    import flash.media.SoundChannel;
    import flash.events.Event;

    var snd:Sound; = new Sound();
    var sndChannel:SoundChannel
    var sndTimer:Timer;

    snd.addEventListener(Event.COMPLETE, soundLoaded);
    snd.load(new URLRequest("soundFile.mp3")); //load after adding the complete event

    function soundLoaded(e:Event):void 
    {
        sndChannel = snd.play();

        //Create a timer to wait 1 second
        sndTimer = new Timer(1000, 1);
        sndTimer.addEventListener(TimerEvent.TIMER, stopSound, false, 0, true);
        sndTimer.start();
    }

    function stopSound(e:Event = null):void {
        sndChannel.stop(); //Stop the sound
    }


## Infinite looping a sound
    import flash.net.URLRequest;
    import flash.media.Sound;
    import flash.events.Event;

    var req:URLRequest = new URLRequest("filename.mp3"); 
    var snd:Sound = new Sound(req);

    snd.addEventListener(Event.COMPLETE, function(e: Event)
    {
        snd.play(0, int.MAX_VALUE); // There is no way to put "infinite"
    }


You also don't need to wait for sound to load before calling `play()` function.
So this will do the same job:

    snd = new Sound(new URLRequest("filename.mp3"));
    snd.play(0, int.MAX_VALUE);

And if you really want to loop sound inifinite time for some reason (`int.MAX_VALUE` will loop 1s sound for about 68 years, not counting the pause an mp3 causes...) you can write something like this:

    var st:SoundChannel = snd.play();
    st.addEventListener(Event.SOUND_COMPLETE, repeat);
    function repeat(e:Event) { 
        st.removeEventListener(Event.SOUND_COMPLETE, repeat);
        (st = snd.play()).addEventListener(Event.SOUND_COMPLETE, repeat);
    }

`play()` function returns new instance of `SoundChannel` object each time it's called. We assgin it to variable and listen for its SOUND_COMPLETE event. In the event callback, listener is removed from current `SoundChannel` object and new one is created for new `SoundChannel` object. 

## Load and play an external sound
    import flash.net.URLRequest;
    import flash.media.Sound;
    import flash.events.Event;

    var req:URLRequest = new URLRequest("click.mp3"); 
    var snd:Sound = new Sound(req);

    snd.addEventListener(Event.COMPLETE, function(e: Event)
    {
        snd.play();
    }

