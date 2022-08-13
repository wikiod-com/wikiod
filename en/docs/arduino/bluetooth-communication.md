---
title: "Bluetooth Communication"
slug: "bluetooth-communication"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Parameters
| method | details|
|-------|----------------------------|
| [SoftwareSerial.h](https://www.arduino.cc/en/Reference/SoftwareSerial) | Documentation|
| SoftwareSerial(rxPin, txPin, inverse_logic) | Constructor. **rxPin**: Data in (receive) pin, defaults to 0. **txPin**: Data out (transmit) pin, defaults to 1. **inverse_logic**: If true, treats LOW as if it were HIGH and HIGH as LOW when determining bit values. defaults to false. |
| begin(speed) | Sets the baud rate for serial communication. Supported baud rates are 300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 28800, 31250, 38400, 57600, and 115200. |
| available() | Check if there is some data over serial |
| read() | Reads a string from serial |
| isListening() | Checks to see if requested software serial port is actively listening. |
| overflow() | Checks if a software serial buffer overflow has occurred. Calling this function clears the overflow flag, meaning that subsequent calls will return false unless another byte of data has been received and discarded in the meantime. The software serial buffer can hold 64 bytes. |
| peek() | Return a character that was received on the RX pin of the software serial port. Unlike read(), however, subsequent calls to this function will return the same character. Note that only one SoftwareSerial instance can receive incoming data at a time (select which one with the `listen()` function). |
| print(data) | Prints data to the transmit pin of the software serial port. Works the same as the `Serial.print()` function. |
| println(data) | Prints data to the transmit pin of the software serial port, followed by a carriage return and line feed. Works the same as the `Serial.println()` function. |
| listen() | Enables the selected software serial port to listen. Only one software serial port can listen at a time; data that arrives for other ports will be discarded. Any data already received is discarded during the call to `listen()` (unless the given instance is already listening). |
| write(data) | Prints data to the transmit pin of the software serial port as raw bytes. Works the same as the `Serial.write()` function. |

Common Mistake : If you keep the rx and tx pins at default values (0 and 1), you cannot upload new code until and unless you remove it, so 
it's almost always better to change the tx and rx pins in the SoftwareSerial constructor.

## Basic bluetooth hello world
    #include <SoftwareSerial.h>
    // its always better to change the default tx and rx as the may interfere with other process in future.
     
    // configure tx , rx by defualt they will be 0 and 1 in arduino UNO  
    SoftwareSerial blue(3,2); 
    void setup() {
      // preferred baud rate/data transfer rate in general is 38400
         blue.begin(38400);
      // do initialization or put one time executing code here
    }
    
    void loop() {

      // put code that you want it to run every time no matter what
        if(blue.available()){
            // put only that code which needsd to run when there is some data
            // This means that the their is some data sent over the bluetooth
            // You can do something with the data
    
            int n;
            // consider that the data received to be integer, read it by using blue.parseInt();
    
            n = blue.parseInt();
    
        }
    
    }



