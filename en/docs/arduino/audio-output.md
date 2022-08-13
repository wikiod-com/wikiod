---
title: "Audio Output"
slug: "audio-output"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Parameters
| Parameter | Details|
| ------ | ------ |
| speaker| Should be an output to an analog speaker|

## Basic Note Outputs
    #define NOTE_C4  262 //From pitches.h file defined in [Arduino Tone Tutorial][1]
    
    int Key = 2;
    int KeyVal = 0;

    byte speaker = 12;
    
    void setup()
    {
      pinMode(Key, INPUT);  //Declare our key (button) as input
      pinMode(speaker, OUTPUT);
    }
    
    void loop()
    {
      KeyVal = digitalRead(Key);
      if (KeyVal == HIGH) {
        tone(speaker, NOTE_C4); //Sends middle C tone out through analog speaker
      } else {
        noTone(speaker); //Ceases tone emitting from analog speaker
      }

      delay(100);
    }


  \[1]: https://www.arduino.cc/en/Tutorial/toneMelody

