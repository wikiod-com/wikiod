---
title: "AVSpeechSynthesizer"
slug: "avspeechsynthesizer"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Syntax


 - AVSpeechSynthesizer() // Creates a speech synthesiser
 - speaker.speakUtterance(speech) // Converts the text to speech

## Parameters
| Parameter | Details |
| --------- |--------- |
| speaker | AVSpeechSynthesizer object |
| speech  | AVSpeechUtterance object |

## Creating a basic text to speech
Use the `speakUtterance:` method of `AVSpeechSynthesizer` to convert text to speech. You need to pass an `AVSpeechUtterance` object to this method, which contains the text that you want to be spoken.


**Objective C**

    AVSpeechSynthesizer *speaker = [[AVSpeechSynthesizer alloc] init];
    AVSpeechUtterance *speech    = [AVSpeechUtterance speechUtteranceWithString:@"Hello World"];
    [speaker speakUtterance:speech];

**Swift**

    let speaker = AVSpeechSynthesizer()
    let speech = AVSpeechUtterance(string: "Hello World")
    speaker.speakUtterance(speech)

