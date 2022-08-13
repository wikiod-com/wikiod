---
title: "How Python integrates with Arduino Uno"
slug: "how-python-integrates-with-arduino-uno"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Syntax
 - `Serial.begin(baudrate)    // Set baud rate (bits per second) for serial data transmission`
 - `Serial.println(value)    // Print data to serial port followed by Carriage Return \r and Newline character \n`
 - `serial.Serial((port=None, baudrate=9600, bytesize=EIGHTBITS, parity=PARITY_NONE, stopbits=STOPBITS_ONE, timeout=None, xonxoff=False, rtscts=False, write_timeout=None, dsrdtr=False, inter_byte_timeout=None)    // Initialize serial port with all parameters`
 - `serial.readline()    // Read serial data which contains Carriage Return \r and Newline character \n`

## Parameters
| Parameter | Details |
| ------ | ------ |
| serial   | Python package contains classes and methods to access serial port   |
| time |  Python package includes time-related functions |

I use an Arduino Uno with Arduino IDE 1.6.9 and Python 2.7.12 running in Windows 10.

## First serial communication between Arduino and Python
In this very first example, a basic serial write operation is started from an Arduino device.

<!-- language: lang-cpp -->

    void setup() {
      // put your setup code here, to run once:
      Serial.begin(9600);
    }
    
    void loop() {
      // put your main code here, to run repeatedly:
      Serial.println("Hello World!");
      delay(100);
    }

In `setup()`, function `Serial.begin(9600)` sets up the baud rate for serial data communication. In this example, a baud rate of 9600 is used. Other values can be read here: [Arduino Serial.begin() function][1]

In `loop()`, the first message we would like to send is "Hello World!". This message is transmitted by using `Serial.println("Hello World!")` as it will send this string to serial port in ASCII format. At the end of the message, there are Carriage Return `(CR, \r)` and Newline character `(\n)`. Also, a delay of 100 milliseconds is used each time program prints to serial port.

Next, upload this Arduino sketch via COM port (remember this COM port number as it will be used in Python program).

The Python program reading serial data sent from Arduino device is shown below:

<!-- language: lang-python -->

    import serial
    import time
    
    ser = serial.Serial('COM8', 9600)
    while (1):
        print ser.readline()
        time.sleep(0.1)

First, pyserial package should be imported. For more information about installing pyserial in Windows environment, please check this instruction: [Installing Python and pyserial][2]. Then, we initialize the serial port with COM port number and baud rate. The baud rate needs to be the same as used in Arduino sketch. 

Received message will be printed in while loop using `readline()` function. A delay of 100 milliseconds is also used here as same as in Arduino sketch. Please notice that pyserial `readline()` function requires a timeout when opening a serial port (pyserial documentation: [PySerial ReadLine][3]).

  [1]: https://www.arduino.cc/en/Serial/Begin
  [2]: https://learn.adafruit.com/arduino-lesson-17-email-sending-movement-detector/installing-python-and-pyserial
  [3]: http://pyserial.readthedocs.io/en/latest/shortintro.html#readline

