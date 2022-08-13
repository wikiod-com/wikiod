---
title: "Analog Inputs"
slug: "analog-inputs"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
- `analogRead(pin)` //Read from the given pin. 

    Serial.println(val)
For help with Serial communication, see: https://www.wikiod.com/arduino/serial-communication

## Print out an Analog Value
    int val = 0;    // variable used to store the value
                    // coming from the sensor
    
    void setup() {
      Serial.begin(9600); //Begin serializer to print out value
    
      // Note: Analogue pins are
      // automatically set as inputs
    }
    
    void loop() {
    
      val = analogRead(0); // read the value from
                           // the sensor connected to A0.

      Serial.println(val); //Prints the value coming in from the analog sensor
     
      delay(10); // stop the program for
                 // some time
    }

## Get Voltage From Analog Pin
Analog pins can be used to read voltages which is useful for battery monitoring or interfacing with analog devices. By default the AREF pin will be the same as the operating voltage of the arduino, but can be set to other values externally. If the voltage to read is larger than the input voltage, a potential devider will be needed to lower the analog voltage.

    #define analogPin 14    //A0 (uno)
    #define AREFValue 5        //Standard for 5V Arduinos
    #define ADCResolution 1023    //Standard for a 10bit ADC

    int ADCValue = 0;
    float voltage = 0;

    void setup()
    {
        Serial.begin(9600);
    }

    void loop() 
    {
        readADC();
        Serial.print(voltage); Serial.println("V");
    }

    void readADC()
    {
        ADCValue = analogRead(analogPin);
        float = ( ( (float)ADCValue/ADCRange ) * AREFValue );    //Convert the ADC value to a float, devide by the ADC resolution and multiply by the AREF voltage
    }

