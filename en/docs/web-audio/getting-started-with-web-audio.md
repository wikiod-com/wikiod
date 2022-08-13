---
title: "Getting started with web-audio"
slug: "getting-started-with-web-audio"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Synthesising audio


## Using effects on audio


## Recording audio from a microphone source


## Playing audio


## Realtime altering of two audio sources


## Setup
We start off by creating an audio context and then create an oscillator which is the easiest way to check that your setup works. [(Example fiddle)](https://jsfiddle.net/xph4w0ey/)

<!-- language: lang-js -->

    // We can either use the standard AudioContext or the webkitAudioContext (Safari)
    var audioContext = (window.AudioContext || window.webkitAudioContext);

    // We create a context from which we will create all web audio nodes
    var context = new audioContext();

    // Create an oscillator and make some noise
    var osc = context.createOscillator();
    
    // set a frequecy, in this case 440Hz which is an A note
    osc.frequency.value = 440;
    
    // connect the oscillator to the context destination (which routes to your speakers)
    osc.connect(context.destination);
    
    // start the sound right away
    osc.start(context.currentTime);
    
    // stop the sound in a second
    osc.stop(context.currentTime + 1);
    


