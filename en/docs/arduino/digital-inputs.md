---
title: "Digital Inputs"
slug: "digital-inputs"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
- `pinMode(pin, pinMode)`       // Sets the pin to the mode defined.
- `digitalRead(pin);`       // Reads the value from a specified digital pin,


## Parameters
| Paramter      | Details|
| ------        | ------ |
| pinmode       | Should be set to `INPUT` or `INPUT_PULLUP`|


If the input pin is not pulled LOW or HIGH, the value will float. That is, it won't be clearly a 1 or a 0, but somewhere in between. For digital input, a pullup or pulldown resistor is a necessity.

## Pushbutton reading
This is an basic example on how to wire up and make an LED turn on/off when the pushbutton is pressed.

![pushbutton](https://www.arduino.cc/en/uploads/Tutorial/PushButton.jpg)

    /* Basic Digital Read
     * ------------------ 
     *
     * turns on and off a light emitting diode(LED) connected to digital  
     * pin 13, when pressing a pushbutton attached to pin 7. It illustrates the
     * concept of Active-Low, which consists in connecting buttons using a
     * 1K to 10K pull-up resistor.
     *
     * Created 1 December 2005
     * copyleft 2005 DojoDave <http://www.0j0.org>
     * http://arduino.berlios.de
     *
     */
    
    int ledPin = 13; // choose the pin for the LED
    int inPin = 7;   // choose the input pin (for a pushbutton)
    int val = 0;     // variable for reading the pin status
    
    void setup() {
      pinMode(ledPin, OUTPUT);  // declare LED as output
      pinMode(inPin, INPUT);    // declare pushbutton as input
    }
    
    void loop(){
      val = digitalRead(inPin);  // read input value
      if (val == HIGH) {         // check if the input is HIGH (button released)
        digitalWrite(ledPin, LOW);  // turn LED OFF
      } else {
        digitalWrite(ledPin, HIGH);  // turn LED ON
      }
    }

Example taken from [Arduino.cc](https://www.arduino.cc/en/tutorial/pushbutton).

