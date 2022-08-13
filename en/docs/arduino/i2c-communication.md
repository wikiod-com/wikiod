---
title: "I2C Communication"
slug: "i2c-communication"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

I2C is a communication protocol that can make two or more Arduino boards talk to each other. The protocol uses two pins - SDA (data line) and SCL (clock line).
Those pins are different from one Arduino board type to another, so check the board specification.
The I2C protocol set one Arduino board as the master, and all the others as a slave. Each slave has a different address that the programmer set hard-coded.
Remark: Make sure all boards connected to the same VCC source

## Multiple slaves
The following example shows how the master can receive data from multiple slaves.
In this example the slave sends two short numbers. The first one is for temperature, and the second one is for moisture. 
Please notice that the temperature is a float (24.3). In order to use only two bytes and not four (float is four bytes), I multiple the temperature in 10, and save it as a short.
So here is the master code:

    #include <Wire.h>
    
    #define BUFFER_SIZE  4
    #define MAX_NUMBER_OF_SLAVES 24
    #define FIRST_SLAVE_ADDRESS 1
    #define READ_CYCLE_DELAY 1000

    byte buffer[BUFFER_SIZE];

    void setup()
    {
      Serial.begin(9600);  
      Serial.println("MASTER READER");
      Serial.println("*************");
      
      Wire.begin();        // Activate I2C link
    }

    void loop()
    {
      for (int slaveAddress = FIRST_SLAVE_ADDRESS; 
           slaveAddress <= MAX_NUMBER_OF_SLAVES; 
           slaveAddress++) 
      { 
        Wire.requestFrom(slaveAddress, BUFFER_SIZE);    // request data from the slave
        if(Wire.available() == BUFFER_SIZE)
          {  // if the available data size is same as I'm expecting
            // Reads the buffer the slave sent
            for (int i = 0; i < BUFFER_SIZE; i++) 
            {
              buffer[i] = Wire.read();  // gets the data
            }

            // Parse the buffer
            // In order to convert the incoming bytes info short, I use union
            union short_tag {
              byte b[2];
              short val;
            } short_cast;
    
            // Parse the temperature
            short_cast.b[0] = buffer[0];
            short_cast.b[1] = buffer[1];
            float temperature = ((float)(short_cast.val)) / 10;
    
            // Parse the moisture
            short_cast.b[0] = buffer[2];
            short_cast.b[1] = buffer[3];
            short moisture = short_cast.val;

            // Prints the income data
            Serial.print("Slave address ");      
            Serial.print(slaveAddress);
            Serial.print(": Temprature = ");
            Serial.print(temprature);
            Serial.print("; Moisture = ");
            Serial.println(moisture);
          }
        }
        Serial.println("*************************");
      
        delay(READ_CYCLE_DELAY);
      }
    }


And now the slave code:

    #include <Wire.h>
    #include <OneWire.h>
    #include <DallasTemperature.h>
    
    //=====================
    // This is the hard-coded address. Change it from one device to another
    #define SLAVE_ADDRESS 1
    //=====================
    
    // I2C Variables
    #define BUFFER_SIZE 2
    #define READ_CYCLE_DELAY 1000
    short data[BUFFER_SIZE];
    
    // Temprature Variables
    OneWire oneWire(8);
    DallasTemperature temperatureSensors(&oneWire);
    float m_temperature;
    
    // Moisture Variables
    short m_moisture;
    
    // General Variables
    int m_timestamp;
    
    void setup()
    {
      Serial.begin(9600);  
      Serial.println("SLAVE SENDER");
      Serial.print("Node address: ");
      Serial.println(SLAVE_ADDRESS);
      Serial.print("Buffer size: ");
      Serial.println(BUFFER_SIZE * sizeof(short));
      Serial.println("***********************");
    
      m_timestamp = millis();
      Wire.begin(NODE_ADDRESS);  // Activate I2C network
      Wire.onRequest(requestEvent); // Set the request event handler
      temperatureSensors.begin();
    }
    
    void loop()
    { 
      if(millis() - m_timestamp < READ_CYCLE_DELAY) return;
    
      // Reads the temperature
      temperatureSensors.requestTemperatures();
      m_temperature = temperatureSensors.getTempCByIndex(0);
    
      // Reads the moisture
      m_moisture = analogRead(A0);
    }
    
    void requestEvent()
    {
      data[0] = m_temperature * 10; // In order to use short, I multiple by 10
      data[1] = m_moisture;
      Wire.write((byte*)data, BUFFER_SIZE * sizeof(short));  
    }

