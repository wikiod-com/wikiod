---
title: "Servo"
slug: "servo"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

A Servo is a an enclosed system containing a motor and some supporting circuitry. The shaft of a servo can be rotated to a fixed angle within an arc using a control signal. If the control signal is maintained, then the servo will maintain its angle.   Servos can easily be controlled with the Arduino `Servo.h` library.

## Syntax
 - #include <Servo.h> // Include the Servo library
 - Servo.attach(pin) // Attach to the servo on pin. Returns a Servo object
 - Servo.write(degrees) // Degrees to move to (0 - 180)
 - Servo.read() // Gets the current rotation of the servo

## Moving the servo back and forth
````
#include <Servo.h>

Servo srv;

void setup() {
  srv.attach(9); // Attach to the servo on pin 9  

}
````
To use a servo, you need to call `attach()` function first. It starts generating a PWM signal controlling a servo on a specified pin. On boards other than Arduino Mega, use of Servo library disables analogWrite() (PWM) functionality on pins 9 and 10, whether or not there is a Servo on those pins.
````
void loop() {
  Servo.write(90); // Move the servo to 90 degrees
  delay(1000); // Wait for it to move to it's new position
  Servo.write(0); // Move the servo to 0 degrees
  delay(1000); // Wait for it to move to it's new position
}
````
Note that you are not guaranteed that the servo reached the desired position, nor you can check it from the program. 

