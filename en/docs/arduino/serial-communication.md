---
title: "Serial Communication"
slug: "serial-communication"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Syntax
- `Serial.begin(speed)` // Opens the serial port on the given baud rate
- `Serial.begin(speed, config)`
- `Serial[1-3].begin(speed)` // **Arduino Mega only!** When writing 1-3 it means you can choose between the numbers 1 to 3 when choosing the serial port. 
- `Serial[1-3].begin(speed, config)` // **Arduino Mega only!** When writing 1-3 it means you can choose between the numbers 1 to 3 when choosing the serial port. 
- `Serial.peek()` // Reads the next byte of input without removing it from the buffer
- `Serial.available()` // Gets the number of bytes in the buffer
- `Serial.print(text)` // Writes text to the serial port
- `Serial.println(text)` // Same as `Serial.print()` but with a trailing newline


## Parameters
| Parameter | Details |
| --------- | ------- |
| Speed     | The rate of the serial port (usually 9600) |
| Text      | The text to write to the serial port (any data type) |
| Data bits | Number of data bits in a packet (from 5 - 8), default is 8 |
| Parity    | Parity options for error detection: none (default), even, odd |
| Stop bits | Number of stop bits in a packet: one (default), two |

The Arduino Mega has four serial ports which there can be choosed from. They are accesed in the following way

      Serial.begin(9600);
      Serial1.begin(38400);
      Serial2.begin(19200);
      Serial3.begin(4800);


The serial port on an Arduino can be set with additional parameters. The config parameter sets data bits, parity, and stop bits. For example:


8 data bits, even parity and 1 stop bit would be - `SERIAL_8E1`

6 data bits, odd parity and 2 stop bit would be - `SERIAL_6O2`

7 data bits, no parity and 1 stop bit would be - `SERIAL_7N1`

## Base64 filtering for serial input data
<!-- language: lang-cpp -->

    String base64="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

    void setup() {

        Serial.begin(9600); // Turn the serial protocol ON
        Serial.println("Start Typing");
    }

    void loop() {

        if (Serial.available() > 0) { // Check if data has been sent from the user
            char c = Serial.read();   // Gets one byte/Character from serial buffer
            int result = base64.indexOf(c); // Base64 filtering
            if (result>=0)
                Serial.print(c); // Only print Base64 string
        }
    }



## Simple read and write
This example listens for input coming in over the serial connection, then repeats it back out the same connection.

<!-- language: lang-cpp -->

    byte incomingBytes;

    void setup() {                
      Serial.begin(9600); // Opens serial port, sets data rate to 9600 bps.
    }
    
    void loop() {
      // Send data only when you receive data.
      if (Serial.available() > 0) {
        // Read the incoming bytes.
        incomingBytes = Serial.read();

        // Echo the data.
        Serial.println(incomingBytes);
      }
    }

## Command Handling over Serial
<!-- language: lang-cpp -->

    byte incoming;
    String inBuffer;

    void setup() {
        Serial.begin(9600); // or whatever baud rate you would like
    }
    
    void loop(){
        // setup as non-blocking code
        if(Serial.available() > 0) {
            incoming = Serial.read();
            
            if(incoming == '\n') {  // newline, carriage return, both, or custom character
            
                // handle the incoming command
                handle_command();

                // Clear the string for the next command
                inBuffer = "";
            } else{
                // add the character to the buffer
                inBuffer += incoming;
            }
        }

        // since code is non-blocking, execute something else . . . .
    }

    void handle_command() {
        // expect something like 'pin 3 high'
        String command = inBuffer.substring(0, inBuffer.indexOf(' '));
        String parameters = inBuffer.substring(inBuffer.indexOf(' ') + 1);
        
        if(command.equalsIgnoreCase('pin')){
            // parse the rest of the information
            int pin = parameters.substring("0, parameters.indexOf(' ')).toInt();
            String state = parameters.substring(parameters.indexOf(' ') + 1);

            if(state.equalsIgnoreCase('high')){
                digitalWrite(pin, HIGH);
            }else if(state.equalsIgnoreCase('low)){
                digitalWrite(pin, LOW);
            }else{
                Serial.println("did not compute");
            }
        } // add code for more commands 
    }

## Serial Communication with Python
If you have an Arduino connected to a computer or a Raspberry Pi, and want to send data from the Arduino to the PC you can do the following:

# Arduino:

<!-- language: c++ -->
    void setup() {
      // Opens serial port, sets data rate to 9600 bps:
      Serial.begin(9600);
    }
    
    void loop() {
      // Sends a line over serial:
      Serial.println("Hello, Python!");
      delay(1000);
    }

# Python:

<!-- language: python -->

    import serial
    
    
    ser = serial.Serial('/dev/ttyACM0', 9600)  # Start serial communication
    while True:
        data = ser.readline()  # Wait for line from Arduino and read it
        print("Received: '{}'".format(data))  # Print the line to the console



