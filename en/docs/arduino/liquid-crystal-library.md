---
title: "Liquid Crystal Library"
slug: "liquid-crystal-library"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Arduino's `Liquid Crystal Library` is a library for controlling LCD displays compatible the Hitachi HD44780 driver, characterised by their 16 pin interface. The 16 pins might be connected via an I2C interface. These displays contain a matrix of 5x7 pixel blocks used to display characters or small monochromatic images. The displays are usually named according to how many rows and columns they have, e.g. 16x2 or 1602 for 16 columns and 2 rows, and 20x4 or 2004 for 20 columns and 4 rows.

## Syntax
- #include <LiquidCrystal.h> // Includes the library
- LiquidCrystal(rs, enable, d4, d5, d6, d7) // 
- LiquidCrystal(rs, rw, enable, d4, d5, d6, d7) 
- LiquidCrystal(rs, enable, d0, d1, d2, d3, d4, d5, d6, d7) 
- LiquidCrystal(rs, rw, enable, d0, d1, d2, d3, d4, d5, d6, d7)

## Parameters
|LiquidCrystal Parameter|Details|
|---|---|
|rs|the number of the Arduino pin that is connected to the RS pin on the LCD|
|rw|the number of the Arduino pin that is connected to the RW pin on the LCD (optional)|
|enable|the number of the Arduino pin that is connected to the enable pin on the LCD|
|d0 - d7|the numbers of the Arduino pins that are connected to the corresponding data pins on the LCD. d0, d1, d2, and d3 are optional; if omitted, the LCD will be controlled using only the four data lines (d4, d5, d6, d7).|

## Basic Usage
    /*
      Wiring:
       LCD pin 1 (VSS) -> Arduino Ground
       LCD pin 2 (VDD) -> Arduino 5V
       LCD pin 3 (VO)  -> Arduino Ground
       LCD pin 4 (RS)  -> Arduino digital pin 12
       LCD pin 5 (RW)  -> Arduino Ground
       LCD pin 6 (E)   -> Arduino digital pin 11
       LCD pin 11 (D4) -> Arduino digital pin 5
       LCD pin 12 (D5) -> Arduino digital pin 4
       LCD pin 13 (D6) -> Arduino digital pin 3
       LCD pin 14 (D7) -> Arduino digital pin 2
    */

    #include <LiquidCrystal.h> // include the library
    
    // initialize the library with the numbers of the interface pins
    LiquidCrystal lcd(12, 11, 5, 4, 3, 2);
    
    void setup() {
      // set up the LCD's number of columns and rows:
      lcd.begin(16, 2);
      // start writing on the first row and first column.
      lcd.setCursor(0, 0);
      // Print a message to the LCD.
      lcd.print("hello, world!");
    }
    
    void loop() {
      // No need to do anything to keep the text on the display
    }

