---
title: "Interrupts"
slug: "interrupts"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntax
 - digitalPinToInterrupt(pin); // converts a pin id to an interrupt id, for use with `attachInterrupt()` and `detachInterrupt()`.

 - attachInterrupt(digitalPinToInterrupt(pin), ISR, mode); // recommended
 - attachInterrupt(interrupt, ISR, mode); // not recommended

 - detachInterrupt(digitalPinToInterrupt(pin));
 - detachInterrupt(interrupt);

 - noInterrupts(); // disables interrupts
 - interrupts(); // re-enable interrupts after `noInterrupts()` has been called.

## Parameters
| Parameter | Notes|
| ------ | ------ |
| interrupt | Id of the interrupt. Not to be mistaken for pin number. |
| ISR | Interrupt Service Routine. This is the method which will be executed when the interrupt occurs. |
| mode | What should cause the interrupt to trigger. One of LOW, CHANGE, RISING, or FALLING. Due boards also allow HIGH. |

Interrupt Service Routines (ISRs) should be as short as possible, since they pause main program execution and can thus screw up time-dependent code. Generally this means in the ISR you set a flag and exit, and in the main program loop you check the flag and do whatever that flag is supposed to do.

You cannot use `delay()` or `millis()` in an ISR because those methods themselves rely on interrupts.

## Interrupt on Button Press
This example uses a push button (tact switch) attached to digital pin 2 and GND, using an internal pull-up resistor so pin 2 is HIGH when the button is not pressed.


    const int LED_PIN = 13;
    const int INTERRUPT_PIN = 2;
    volatile bool ledState = LOW;
    
    void setup() {
        pinMode(LED_PIN, OUTPUT);
        pinMode(INTERRUPT_PIN, INPUT_PULLUP);
        attachInterrupt(digitalPinToInterrupt(INTERRUPT_PIN), myISR, FALLING); // trigger when button pressed, but not when released.
    }
    
    void loop() {
        digitalWrite(LED_PIN, ledState);
    }
    
    void myISR() {
        ledState = !ledState;
        // note: LOW == false == 0, HIGH == true == 1, so inverting the boolean is the same as switching between LOW and HIGH.
    }

One gotcha with this simple example is that push buttons tend to bounce, meaning that when pressing or releasing, the circuit opens and closes more than once before it settles into the final closed or open state. This example doesn't take that into account. As a result, sometimes pressing the button will toggle the LED multiple times, instead of the expected once.

