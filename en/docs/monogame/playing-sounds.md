---
title: "Playing Sounds"
slug: "playing-sounds"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Sounds using SoundEffect
<!-- language-all: lang-cs -->

In order to play a sound using the [`SoundEffect`][1] type, create a variable to hold the loaded sound. Typically this would be an instance variable in the [`Game`][2] class:

    private SoundEffect mySound;

Then, in the [`LoadContent()`][3] method of the `Game` class:

    protected override void LoadContent()
    {
        // load the audio content
        mySound = Content.Load("mySound");
    }

Finally, whenever the sound needs to be played, just invoke the [`Play()`][4] method:

    bool played = mySound.Play();

If for some reason, such as too many sounds are already playing, the `Play()` method may return `false`. If the sound started playing successfully, then it will return `true`.


  [1]: http://www.monogame.net/documentation/?page=T_Microsoft_Xna_Framework_Audio_SoundEffect
  [2]: http://www.monogame.net/documentation/?page=T_Microsoft_Xna_Framework_Game
  [3]: http://www.monogame.net/documentation/?page=M_Microsoft_Xna_Framework_Game_LoadContent
  [4]: http://www.monogame.net/documentation/?page=M_Microsoft_Xna_Framework_Audio_SoundEffect_Play_1

## Controlling the playback using SoundEffectInstance
[`SoundEffect.Play()`][1] plays the sound effect in a "fire-and-forget" fashion. The sound plays once and its lifetime is managed by the framework. You are not able to change the properties (volume, pan, pitch) of the sound during playback, loop it, position it in 3D or pause it.

You can hold a reference to the playing sound by creating a [`SoundEffectInstance`][2]. Instead of calling `SoundEffect.Play()`, call [`CreateInstance()`][3] on the `SoundEffect` and then [`Play()`][4] on the new instance:

    SoundEffectInstance instance = mySound.CreateInstance();
    
    // Set some properties
    instance.Pitch = 1.0f;
    instance.IsLooped = true;

    // Play the sound effect
    instance.Play();

There can be multiple instances of the same `SoundEffect`, each with their own properties. The instance can be replayed by calling `Play()` after the playback has stopped.


  [1]: http://www.monogame.net/documentation/?page=M_Microsoft_Xna_Framework_Audio_SoundEffect_Play_1
  [2]: http://www.monogame.net/documentation/?page=T_Microsoft_Xna_Framework_Audio_SoundEffectInstance
  [3]: http://www.monogame.net/documentation/?page=M_Microsoft_Xna_Framework_Audio_SoundEffect_CreateInstance
  [4]: http://www.monogame.net/documentation/?page=M_Microsoft_Xna_Framework_Audio_SoundEffectInstance_Play

