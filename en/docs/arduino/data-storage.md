---
title: "Data Storage"
slug: "data-storage"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## cardInfo
<!-- language: lang-cpp -->

    /*
     SD card test
    
     This example shows how use the utility libraries on which the'
     SD library is based in order to get info about your SD card.
     Very useful for testing a card when you're not sure whether its working or not.
    
     The circuit:
      * SD card attached to SPI bus as follows:
     ** MOSI - pin 11 on Arduino Uno/Duemilanove/Diecimila
     ** MISO - pin 12 on Arduino Uno/Duemilanove/Diecimila
     ** CLK -  pin 13 on Arduino Uno/Duemilanove/Diecimila
     ** CS -   depends on your SD card shield or module.
               Pin 4 used here for consistency with other Arduino examples
    
     created  28 Mar 2011
     by Limor Fried
     modified 9 Apr 2012
     by Tom Igoe
     */

    // include the SD library:
    #include <SPI.h>
    #include <SD.h>
    
    // set up variables using the SD utility library functions:
    Sd2Card card;
    SdVolume volume;
    SdFile root;
    
    // change this to match your SD shield or module;
    // Arduino Ethernet shield: pin 4
    // Adafruit SD shields and modules: pin 10
    // Sparkfun SD shield: pin 8
    const int chipSelect = 4;
    
    void setup()
    {
      // Open serial communications and wait for port to open:
      Serial.begin(9600);
      while (!Serial) {
        ; // wait for serial port to connect. Needed for Leonardo only
      }
   
      Serial.print("\nInitializing SD card...");
    
      // we'll use the initialization code from the utility libraries
      // since we're just testing if the card is working!
      if (!card.init(SPI_HALF_SPEED, chipSelect)) {
        Serial.println("initialization failed. Things to check:");
        Serial.println("* is a card inserted?");
        Serial.println("* is your wiring correct?");
        Serial.println("* did you change the chipSelect pin to match your shield or module?");
        return;
      } else {
        Serial.println("Wiring is correct and a card is present.");
      }
    
      // print the type of card
      Serial.print("\nCard type: ");
      switch (card.type()) {
        case SD_CARD_TYPE_SD1:
          Serial.println("SD1");
          break;
        case SD_CARD_TYPE_SD2:
          Serial.println("SD2");
          break;
        case SD_CARD_TYPE_SDHC:
          Serial.println("SDHC");
          break;
        default:
          Serial.println("Unknown");
      }
    
      // Now we will try to open the 'volume'/'partition' - it should be FAT16 or FAT32
      if (!volume.init(card)) {
        Serial.println("Could not find FAT16/FAT32 partition.\nMake sure you've formatted the card");
        return;
      }
    
      // print the type and size of the first FAT-type volume
      uint32_t volumesize;
      Serial.print("\nVolume type is FAT");
      Serial.println(volume.fatType(), DEC);
      Serial.println();
    
      volumesize = volume.blocksPerCluster();    // clusters are collections of blocks
      volumesize *= volume.clusterCount();       // we'll have a lot of clusters
      volumesize *= 512;                            // SD card blocks are always 512 bytes
      Serial.print("Volume size (bytes): ");
      Serial.println(volumesize);
      Serial.print("Volume size (Kbytes): ");
      volumesize /= 1024;
      Serial.println(volumesize);
      Serial.print("Volume size (Mbytes): ");
      volumesize /= 1024;
      Serial.println(volumesize);
    
    
      Serial.println("\nFiles found on the card (name, date and size in bytes): ");
      root.openRoot(volume);
    
      // list all files in the card with date and size
      root.ls(LS_R | LS_DATE | LS_SIZE);
    }
    
    void loop(void) {
    
    }

## SD card datalogger
<!-- language: lang-cpp -->

    /*
      SD card datalogger
    
     This example shows how to log data from three analog sensors
     to an SD card using the SD library.
    
     The circuit:
     * analog sensors on analog ins 0, 1, and 2
     * SD card attached to SPI bus as follows:
     ** MOSI - pin 11
     ** MISO - pin 12
     ** CLK - pin 13
     ** CS - pin 4
    
     created  24 Nov 2010
     modified 9 Apr 2012
     by Tom Igoe
    
     This example code is in the public domain.
    
     */
    
    #include <SPI.h>
    #include <SD.h>
    
    const int chipSelect = 4;
    
    void setup()
    {
      // Open serial communications and wait for port to open:
      Serial.begin(9600);
      while (!Serial) {
        ; // wait for serial port to connect. Needed for Leonardo only
      }
    
      Serial.print("Initializing SD card...");
    
      // see if the card is present and can be initialized:
      if (!SD.begin(chipSelect)) {
        Serial.println("Card failed, or not present");
        // don't do anything more:
        return;
      }
      Serial.println("card initialized.");
    }
    
    void loop()
    {
      // make a string for assembling the data to log:
      String dataString = "";
    
      // read three sensors and append to the string:
      for (int analogPin = 0; analogPin < 3; analogPin++) {
        int sensor = analogRead(analogPin);
        dataString += String(sensor);
        if (analogPin < 2) {
          dataString += ",";
        }
      }
    
      // open the file. note that only one file can be open at a time,
      // so you have to close this one before opening another.
      File dataFile = SD.open("datalog.txt", FILE_WRITE);
    
      // if the file is available, write to it:
      if (dataFile) {
        dataFile.println(dataString);
        dataFile.close();
        // print to the serial port too:
        Serial.println(dataString);
      }
      // if the file isn't open, pop up an error:
      else {
        Serial.println("error opening datalog.txt");
      }
    }

## SD card file dump
<!-- language: lang-cpp -->

    /*
      SD card file dump
    
     This example shows how to read a file from the SD card using the
     SD library and send it over the serial port.
    
     The circuit:
     * SD card attached to SPI bus as follows:
     ** MOSI - pin 11
     ** MISO - pin 12
     ** CLK - pin 13
     ** CS - pin 4
    
     created  22 December 2010
     by Limor Fried
     modified 9 Apr 2012
     by Tom Igoe
    
     This example code is in the public domain.
    
     */
    
    #include <SPI.h>
    #include <SD.h>
    
    const int chipSelect = 4;
    
    void setup()
    {
      // Open serial communications and wait for port to open:
      Serial.begin(9600);
      while (!Serial) {
        ; // wait for serial port to connect. Needed for Leonardo only
      }
    
    
      Serial.print("Initializing SD card...");
    
      // see if the card is present and can be initialized:
      if (!SD.begin(chipSelect)) {
        Serial.println("Card failed, or not present");
        // don't do anything more:
        return;
      }
      Serial.println("card initialized.");
    
      // open the file. note that only one file can be open at a time,
      // so you have to close this one before opening another.
      File dataFile = SD.open("datalog.txt");
    
      // if the file is available, write to it:
      if (dataFile) {
        while (dataFile.available()) {
          Serial.write(dataFile.read());
        }
        dataFile.close();
      }
      // if the file isn't open, pop up an error:
      else {
        Serial.println("error opening datalog.txt");
      }
    }
    
    void loop()
    {
      
    }

## SD card basic file example
<!-- language: lang-cpp -->

    /*
      SD card basic file example
    
     This example shows how to create and destroy an SD card file
     The circuit:
     * SD card attached to SPI bus as follows:
     ** MOSI - pin 11
     ** MISO - pin 12
     ** CLK - pin 13
     ** CS - pin 4
    
     created   Nov 2010
     by David A. Mellis
     modified 9 Apr 2012
     by Tom Igoe
    
     This example code is in the public domain.
    
     */
    #include <SPI.h>
    #include <SD.h>
    
    File myFile;
    
    void setup()
    {
      // Open serial communications and wait for port to open:
      Serial.begin(9600);
      while (!Serial) {
        ; // wait for serial port to connect. Needed for Leonardo only
      }
    
      Serial.print("Initializing SD card...");
    
      if (!SD.begin(4)) {
        Serial.println("initialization failed!");
        return;
      }
      Serial.println("initialization done.");
    
      if (SD.exists("example.txt")) {
        Serial.println("example.txt exists.");
      }
      else {
        Serial.println("example.txt doesn't exist.");
      }
    
      // open a new file and immediately close it:
      Serial.println("Creating example.txt...");
      myFile = SD.open("example.txt", FILE_WRITE);
      myFile.close();
    
      // Check to see if the file exists:
      if (SD.exists("example.txt")) {
        Serial.println("example.txt exists.");
      }
      else {
        Serial.println("example.txt doesn't exist.");
      }
    
      // delete the file:
      Serial.println("Removing example.txt...");
      SD.remove("example.txt");
    
      if (SD.exists("example.txt")) {
        Serial.println("example.txt exists.");
      }
      else {
        Serial.println("example.txt doesn't exist.");
      }
    }
    
    void loop()
    {
      // nothing happens after setup finishes.
    }

## Listfiles
<!-- language: lang-cpp -->    

    /*
     Listfiles
     
     This example shows how print out the files in a 
     directory on a SD card 
         
     The circuit:
     * SD card attached to SPI bus as follows:
     ** MOSI - pin 11
     ** MISO - pin 12
     ** CLK - pin 13
     ** CS - pin 4
    
     created   Nov 2010
     by David A. Mellis
     modified 9 Apr 2012
     by Tom Igoe
     modified 2 Feb 2014
     by Scott Fitzgerald
     
     This example code is in the public domain.
    */

    #include <SPI.h>
    #include <SD.h>
    
    File root;
    
    void setup()
    {
      // Open serial communications and wait for port to open:
      Serial.begin(9600);
      while (!Serial) {
        ; // wait for serial port to connect. Needed for Leonardo only
      }
    
      Serial.print("Initializing SD card...");
    
      if (!SD.begin(4)) {
        Serial.println("initialization failed!");
        return;
      }
      Serial.println("initialization done.");
    
      root = SD.open("/");
    
      printDirectory(root, 0);
    
      Serial.println("done!");
    }
    
    void loop()
    {
      // nothing happens after setup finishes.
    }
    
    void printDirectory(File dir, int numTabs) {
       while(true) {
         
         File entry =  dir.openNextFile();
         if (! entry) {
           // no more files
           break;
         }
         for (uint8_t i=0; i<numTabs; i++) {
           Serial.print('\t');
         }
         Serial.print(entry.name());
         if (entry.isDirectory()) {
           Serial.println("/");
           printDirectory(entry, numTabs+1);
         } else {
           // files have sizes, directories do not
           Serial.print("\t\t");
           Serial.println(entry.size(), DEC);
         }
         entry.close();
       }
    }

## SD card read/write
<!-- language: lang-cpp -->

    /*
      SD card read/write
    
     This example shows how to read and write data to and from an SD card file
     The circuit:
     * SD card attached to SPI bus as follows:
     ** MOSI - pin 11
     ** MISO - pin 12
     ** CLK - pin 13
     ** CS - pin 4
    
     created   Nov 2010
     by David A. Mellis
     modified 9 Apr 2012
     by Tom Igoe
    
     This example code is in the public domain.
    
     */
    
    #include <SPI.h>
    #include <SD.h>
    
    File myFile;
    
    void setup()
    {
      // Open serial communications and wait for port to open:
      Serial.begin(9600);
      while (!Serial) {
        ; // wait for serial port to connect. Needed for Leonardo only
      }
    
    
      Serial.print("Initializing SD card...");
    
      if (!SD.begin(4)) {
        Serial.println("initialization failed!");
        return;
      }
      Serial.println("initialization done.");
    
      // open the file. note that only one file can be open at a time,
      // so you have to close this one before opening another.
      myFile = SD.open("test.txt", FILE_WRITE);
    
      // if the file opened okay, write to it:
      if (myFile) {
        Serial.print("Writing to test.txt...");
        myFile.println("testing 1, 2, 3.");
        // close the file:
        myFile.close();
        Serial.println("done.");
      } else {
        // if the file didn't open, print an error:
        Serial.println("error opening test.txt");
      }
    
      // re-open the file for reading:
      myFile = SD.open("test.txt");
      if (myFile) {
        Serial.println("test.txt:");
    
        // read from the file until there's nothing else in it:
        while (myFile.available()) {
          Serial.write(myFile.read());
        }
        // close the file:
        myFile.close();
      } else {
        // if the file didn't open, print an error:
        Serial.println("error opening test.txt");
      }
    }
    
    void loop()
    {
      // nothing happens after setup
    }



