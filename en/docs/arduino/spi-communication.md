---
title: "SPI Communication"
slug: "spi-communication"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

Chip select signals
=

Most slaves have an active low chip select input. So proper code to initialize and use a chip select pin is this:


    #define CSPIN 1 // or whatever else your CS pin is
    // init:
    pinMode(CSPIN, OUTPUT);
    digitalWrite(CSPIN, 1); // deselect

    // use:
    digitalWrite(CSPIN, 0); // select
    ... perform data transfer ...
    digitalWrite(CSPIN, 1); // deselect

Deselecting a slave is just as important as selecting it, because a slave may drive the MISO line while it is selected. There may be many slaves, but only one may drive MISO. If a slave is not deselected properly, two or more slaves might be driving MISO, which may lead to shorts between their outputs and might damage the devices.

Transactions
=

Transactions serve two purposes:
- tell the SPI when we want to start and end using it within a particular context
- configure the SPI for a specific chip

The clock line has different idle states in the different SPI modes. Changing the SPI mode while a slave is selected might confuse the slave, so always set the SPI mode before selecting a slave. The SPI mode can be set with an `SPISettings` object passed to `SPI.beginTransaction`:

    SPI.beginTransaction(SPISettings(1000000, MSBFIRST, SPI_MODE0));
    digitalWrite(CSPIN, 0);
    ... perform data transfer ...
    digitalWrite(CSPIN, 1);
    SPI.endTransaction();

`SPISettings` may also be stored elsewhere:

    SPISettings mySettings(1000000, MSBFIRST, SPI_MODE0);
    SPI.beginTransaction(mySettings);

If another part of the code tries to use the SPI between a pair of calls to `beginTransaction()` and `endTransaction()`, an error may be raised - how that is done depends on the implementation.

Also see [Arduino Reference: SPISettings][1]

Using the SPI in Interrupt Service Routines
=
If the SPI has to be used within an ISR, no other transaction may be taking place at the same time. The SPI library provides `usingInterrupt(interrupt_number)` to facilitate this. It works by disabling the given interrupt whenever `beginTransaction()` is called, so the interrupt cannot fire between that pair fo calls to `beginTransaction()` and `endTransaction()`.

Also see [Arduino Reference: SPI: usingInterrupt][2]


  [1]: https://www.arduino.cc/en/Reference/SPISettings
  [2]: https://www.arduino.cc/en/Reference/SPIusingInterrupt

## Basics: initialize the SPI and a chip select pin, and perform a 1-byte transfer
    #include <SPI.h>
    #define CSPIN 1
    
    void setup() {
      pinMode(CSPIN, OUTPUT); // init chip select pin as an output
      digitalWrite(CSPIN, 1); // most slaves interpret a high level on CS as "deasserted"
    
      SPI.begin();
    
      SPI.beginTransaction(SPISettings(1000000, MSBFIRST, SPI_MODE0));
      digitalWrite(CSPIN, 0);

      unsigned char sent = 0x01;
      unsigned char received = SPI.transfer(sent);
      // more data could be transferred here

      digitalWrite(CSPIN, 1);
      SPI.endTransaction();
    
      SPI.end();
    }
    
    void loop() {
      // we don't need loop code in this example.
    }

This example:

- properly initializes and uses a chip select pin (see remarks)
- properly uses an SPI transaction (see remarks)
- only uses the SPI to transfer one single byte. There is also a method for transferring arrays, which is not used here.



