---
title: "Audio System"
slug: "audio-system"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

This is a documentation about playing audio in Unity3D.

## Audio class - Play audio
    using UnityEngine;

    public class Audio : MonoBehaviour {
        AudioSource audioSource;
        AudioClip audioClip;
        
        void Start() {
            audioClip = (AudioClip)Resources.Load("Audio/Soundtrack");
            audioSource.clip = audioClip;
            if (!audioSource.isPlaying) audioSource.Play();
        }

