---
title: "Digital Output"
slug: "digital-output"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- `digitalWrite(pin, value)`

## Write to pin
    int ledPin = 13;                 // LED connected to digital pin 13
    
    void setup()
    {
      pinMode(ledPin, OUTPUT);      // sets the digital pin as output
    }
    
    void loop()
    {
      digitalWrite(ledPin, HIGH);   // sets the LED on
      delay(1000);                  // waits for a second
      digitalWrite(ledPin, LOW);    // sets the LED off
      delay(1000);                  // waits for a second
    }
Example at [Arduino.cc][1].


  [1]: https://www.arduino.cc/en/Reference/DigitalWrite

