---
title: "AudioManager"
slug: "audiomanager"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Requesting Transient Audio Focus
    audioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);

    audioManager.requestAudioFocus(audioListener, AudioManager.STREAM_MUSIC, AudioManager.AUDIOFOCUS_GAIN_TRANSIENT);

    changedListener = new AudioManager.OnAudioFocusChangeListener() {
        @Override
        public void onAudioFocusChange(int focusChange) {
            if (focusChange == AudioManager.AUDIOFOCUS_GAIN) {
                // You now have the audio focus and may play sound.
                // When the sound has been played you give the focus back.
                audioManager.abandonAudioFocus(changedListener);
            }
        }
    }

## Requesting Audio Focus
    audioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);

    audioManager.requestAudioFocus(audioListener, AudioManager.STREAM_MUSIC, AudioManager.AUDIOFOCUS_GAIN);

    changedListener = new AudioManager.OnAudioFocusChangeListener() {
        @Override
        public void onAudioFocusChange(int focusChange) {
            if (focusChange == AudioManager.AUDIOFOCUS_GAIN) {
                // You now have the audio focus and may play sound.
            }
            else if (focusChange == AudioManager.AUDIOFOCUS_REQUEST_FAILED) {
                // Handle the failure.
            }
        }
    }

