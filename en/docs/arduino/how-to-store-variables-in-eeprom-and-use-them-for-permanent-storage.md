---
title: "How to store variables in EEPROM and use them for permanent storage"
slug: "how-to-store-variables-in-eeprom-and-use-them-for-permanent-storage"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Syntax


 - EEPROM.write(address, value); //(Store variables in EEPROM in a particular address)
 - EEPROM.read(address); //(Retrieve values from EEPROM and read data stored in EEPROM)



## Parameters
| Parameters of EEPROM.write | Detail |
| ------ | ------ |
| address  | The address where value is to be stored in EEPROM   |
| value  |  Main variable to store in EEPROM. Note that this is a `uint_8` (single byte)—you must split multiple-byte data types into single bytes yourself. Or you can use `EEPROM.put` to store floats or other data types.  |
| **Parameters of EEPROM.Read** | Detail |
| address   | The address from which the variable is to be read   |

The allowable addresses vary by hardware. 

 - ATMega328 (Uno, Pro Mini, etc.): 0–1023
 - ATMega168: 0-511
 - ATMega1280: 0-4095
 - ATMega2560: 0-4095

[source](https://www.arduino.cc/en/Reference/EEPROM)


## Store a variable in EEPROM and then retrieve it and print to screen
First, add a reference to `<EEPROM.h>` at the start of your sketch:

<!-- language: lang-cpp -->

    #include <EEPROM.h>

Then your other code:

<!-- language: lang-cpp -->

    // Stores value in a particular address in EEPROM. There are almost 512 addresses present.

        // Store value 24 to Address 0 in EEPROM
        int addr = 0;
        int val = 24;
        EEPROM.write(addr, val);     // Writes 24 to address 0
        
        // ---------
        // Retrieves value from a particular address in EEPROM
        // Retrieve value from address 0 in EEPROM
        int retrievedVal = EEPROM.read(0);    // Retrieves value stored in 0 address in
                                              // EEPROM
    
        // *[NOTE: put Serial.begin(9600); at void setup()]*
        Serial.println(retrievedVal);        // Prints value stored in EEPROM Address 0 to 
                                             // Serial (screen)




