---
title: "MIDI Communication"
slug: "midi-communication"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The intent of this topic to demonstrate some basic MIDI programs that show how to operate with the protocol and progressively add useful features that more complex applications require.

## MIDI THRU Example
The MIDI Thru is simple and easy to test.  When working properly you will be able to install your Arduino project between two MIDI devices, MIDI IN to MIDI OUT and you will be able to verify that the two device operate together.  If you have the ability to measure latency, you will see an increase due to the serial buffer capture and re-transmit instructions.

    // This is a simple MIDI THRU.  Everything in, goes right out.
    // This has been validate on an Arduino UNO and a Olimex MIDI Shield  
    
    boolean byteReady; 
    unsigned char midiByte;
    
    void setup() {
        // put your setup code here, to run once:
        //  Set MIDI baud rate:
        Serial.begin(31250);
        byteReady = false;
        midiByte = 0;  
    }
    
    // The Loop that always gets called...
    void loop() {
       if (byteReady) {
            byteReady = false;
            Serial.write(midiByte);
        }
    }
    
    // The little function that gets called each time loop is called.  
    // This is automated somwhere in the Arduino code.
    void serialEvent() {
      if (Serial.available()) {
        // get the new byte:
        midiByte = (unsigned char)Serial.read();
        byteReady = true;
      }
    }

## MIDI Thru with Queue


    // This is a more complex MIDI THRU.  This version uses a queue.  Queues are important because some
    // MIDI messages can be interrupted for real time events.  If you are generating your own messages,
    // you may need to stop your message to let a "real time" message through and then resume your message.

    
    #define QUEUE_DEPTH 128
    
    // Queue Logic for storing messages
    int headQ = 0;
    int tailQ = 0;
    unsigned char tx_queue[QUEUE_DEPTH];
    
    void setup() {
        // put your setup code here, to run once:
        //  Set MIDI baud rate:
        Serial.begin(31250);
    }
    
    // getQDepth checks for roll over.  Folks have told me this 
    // is not required.  Feel free to experiment.
    int getQDepth() {
    int depth = 0;
        if (headQ < tailQ) {
            depth = QUEUE_DEPTH - (tailQ - headQ);
        } else {
            depth = headQ - tailQ;
        }
        return depth;
    }
    
    void addQueue (unsigned char myByte) {
        int depth = 0;
        depth = getQDepth();
    
        if (depth < (QUEUE_DEPTH-2)) {
            tx_queue[headQ] = myByte;
            headQ++;
            headQ = headQ % QUEUE_DEPTH; // Always keep the headQ limited between 0 and 127
        }
    }
    
    unsigned char deQueue() {
        unsigned char myByte;
        myByte = tx_queue[tailQ];
        tailQ++;
        tailQ = tailQ % QUEUE_DEPTH;  // Keep this tailQ contained within a limit
        // Now that we dequeed the byte, it must be sent. 
        return myByte;
    }
    
    void loop() {
       if (getQDepth>0) {
            Serial.write(deQueue());
        }
    }
    
    // The little function that gets called each time loop is called.  
    // This is automated somwhere in the Arduino code.
    void serialEvent() {
      if (Serial.available()) {
        // get the new byte:
        addQueue((unsigned char)Serial.read());;
      }
    }




## MIDI Clock Generation
    // This is a MiDI clk generator.  This takes a #defined BPM and 
    // makes the appropriate clk rate.  The queue is used to let other messages 
    // through, but allows a clock to go immediately to reduce clock jitter
    
    #define QUEUE_DEPTH 128
    #define BPM 121
    #define MIDI_SYSRT_CLK 0xF8
    
    // clock tracking and calculation
    unsigned long lastClock;
    unsigned long captClock;
    unsigned long clk_period_us;
    
    // Queue Logic for storing messages
    int headQ = 0;
    int tailQ = 0;
    unsigned char tx_queue[QUEUE_DEPTH];
    
    void setup() {
        //  Set MIDI baud rate:
        Serial.begin(31250);
        clk_period_us = 60000000 / (24 * BPM);
        lastClock = micros();
    }
    
    // getQDepth checks for roll over.  Folks have told me this 
    // is not required.  Feel free to experiment.
    int getQDepth() {
    int depth = 0;
        if (headQ < tailQ) {
            depth = QUEUE_DEPTH - (tailQ - headQ);
        } else {
            depth = headQ - tailQ;
        }
        return depth;
    }
    
    void addQueue (unsigned char myByte) {
        int depth = 0;
        depth = getQDepth();
    
        if (depth < (QUEUE_DEPTH-2)) {
            tx_queue[headQ] = myByte;
            headQ++;
            headQ = headQ % QUEUE_DEPTH; // Always keep the headQ limited between 0 and 127
        }
    }
    
    unsigned char deQueue() {
        unsigned char myByte;
        myByte = tx_queue[tailQ];
        tailQ++;
        tailQ = tailQ % QUEUE_DEPTH;  // Keep this tailQ contained within a limit
        // Now that we dequeed the byte, it must be sent. 
        return myByte;
    }
    
    void loop() {
        captClock = micros();
        
        if (lastClock > captClock) {
            // we have a roll over condition - Again, maybe we don't need to do this.
            if (clk_period_us <= (4294967295 - (lastClock - captClock))) {
                // Add a the ideal clock period for this BPM to the last measurement value
                lastClock = lastClock + clk_period_us;
                // Send a clock, bypasing the transmit queue
                Serial.write(MIDI_SYSRT_CLK);
            }
        } else if (clk_period_us <= captClock-lastClock) {
            // Basically the same two commands above, but not within a roll over check
            lastClock = lastClock + clk_period_us;
            // Send a clock, bypasing the transmit queue
            Serial.write(MIDI_SYSRT_CLK);
        }
    
        if (getQDepth>0) {
            Serial.write(deQueue());
        }
    }
    
    // The little function that gets called each time loop is called.  
    // This is automated somwhere in the Arduino code.
    void serialEvent() {
      if (Serial.available()) {
        // get the new byte:
        addQueue((unsigned char)Serial.read());;
      }
    }



## MIDI Messages Defined
In general, MIDI protocol is broken down into "messages".  There are 4 general classes of messages:

 - Channel Voice
 - Channel Mode
 - System Common
 - System Real-Time Messages

Messages start with a byte value above 0x80.  Any value below 0x7F is considered data.  Effectively meaning that 127 is the maximum value that can be encoded into a single MIDI data byte.  To encode larger values, two or more MIDI data bytes are required.

It should be pointed out that messages must be sent start to finish without interruption... EXCEPT... System Real-Time messages, which are a single byte, which can be injected in the middle of any message.

**Channel Voice Messages**

| Status D7..D0 | Data Bytes           |Description     |
| ------------- | ---------------------|----------------|
| 1000nnnn      | 0kkkkkkk 0vvvvvvv    |Note Off event.This message is sent when a note is released (ended). (kkkkkkk) is the key (note) number. (vvvvvvv) is the velocity.                |
| 1001nnnn | 0kkkkkkk 0vvvvvvv   | Note On event. This message is sent when a note is depressed (start). (kkkkkkk) is the key (note) number. (vvvvvvv) is the velocity.  |
| 1010nnnn   |     0kkkkkkk 0vvvvvvv   |   Polyphonic Key Pressure (Aftertouch). This message is most often sent by pressing down on the key after it "bottoms out". (kkkkkkk) is the key (note) number. (vvvvvvv) is the pressure value.                                                                                                                                                                       |
| 1011nnnn   |     0ccccccc 0vvvvvvv   |   Control Change. This message is sent when a controller value changes. Controllers include devices such as pedals and levers. Controller numbers 120-127 are reserved as "Channel Mode Messages" (below). (ccccccc) is the controller number (0-119). (vvvvvvv) is the controller value (0-127).                                                                      |
| 1100nnnn   |     0ppppppp            |   Program Change. This message sent when the patch number changes. (ppppppp) is the new program number.                                                                                                                                                                                                                                                                |
| 1101nnnn   |     0vvvvvvv            |   Channel Pressure (After-touch). This message is most often sent by pressing down on the key after it "bottoms out". This message is different from polyphonic after-touch. Use this message to send the single greatest pressure value (of all the current depressed keys). (vvvvvvv) is the pressure value.                                                         |
| 1110nnnn   |     0lllllll 0mmmmmmm   |   Pitch Bend Change. This message is sent to indicate a change in the pitch bender (wheel or lever, typically). The pitch bender is measured by a fourteen bit value. Center (no pitch change) is 2000H. Sensitivity is a function of the receiver, but may be set using RPN 0. (lllllll) are the least significant 7 bits. (mmmmmmm) are the most significant 7 bits. |


**Channel Mode Messages**

| Status D7..D0 | Data Bytes           |Description     |
| ------------- | ---------------------|----------------|
|1011nnnn    |   0ccccccc 0vvvvvvv     | Channel Mode Messages. This the same code as the Control Change (above), but implements Mode control and special message by using reserved controller numbers 120-127. The commands are:                                       |
|            |                         |   All Sound Off. When All Sound Off is received all oscillators will turn off, and their volume envelopes are set to zero as soon as possible. c = 120, v = 0: All Sound Off                                                   |
|            |                         |   Reset All Controllers. When Reset All Controllers is received, all controller values are reset to their default values. (See specific Recommended Practices for defaults).                                                   |
|            |                         |   c = 121, v = x: Value must only be zero unless otherwise allowed in a specific Recommended Practice.                                                                                                                         |
|            |                         |   Local Control. When Local Control is Off, all devices on a given channel will respond only to data received over MIDI. Played data, etc. will be ignored. Local Control On restores the functions of the normal controllers. |
|            |                         |   c = 122, v = 0: Local Control Off                                                                                                                                                                                            |
|            |                         |   c = 122, v = 127: Local Control On                                                                                                                                                                                           |
|            |                         |   All Notes Off. When an All Notes Off is received, all oscillators will turn off.                                                                                                                                             |
|            |                         |   c = 123, v = 0: All Notes Off (See text for description of actual mode commands.)                                                                                                                                            |
|            |                         |   c = 124, v = 0: Omni Mode Off                                                                                                                                                                                                |
|            |                         |   c = 125, v = 0: Omni Mode On                                                                                                                                                                                                 |
|            |                         |   c = 126, v = M: Mono Mode On (Poly Off) where M is the number of channels (Omni Off) or 0 (Omni On)                                                                                                                          |
|            |                         |   c = 127, v = 0: Poly Mode On (Mono Off) (Note: These four messages also cause All Notes Off)                                                                                                                                 |

**System Common Messages**

| Status D7..D0 | Data Bytes           |Description     |
| ------------- | ---------------------|----------------|
|11110000      |   0iiiiiii [0iiiiiii   0iiiiiii]    0ddddddd   --- ---  0ddddddd  11110111       |    System Exclusive. This message type allows manufacturers to create their own messages (such as bulk dumps, patch parameters, and other non-spec data) and provides a mechanism for creating additional MIDI Specification messages. The Manufacturer's ID code (assigned by MMA or AMEI) is either 1 byte (0iiiiiii) or 3 bytes (0iiiiiii 0iiiiiii 0iiiiiii). Two of the 1 Byte IDs are reserved for extensions called Universal Exclusive Messages, which are not manufacturer-specific. If a device recognizes the ID code as its own (or as a supported Universal message) it will listen to the rest of the message (0ddddddd). Otherwise, the message will be ignored. (Note: Only Real-Time messages may be interleaved with a System Exclusive.)|                                                                                                                                                                                              |
|11110001     |   0nnndddd            |    MIDI Time Code Quarter Frame. nnn = Message Type  dddd = Values                                                                                                                             |
|11110010     |   0lllllll 0mmmmmmm   |    Song Position Pointer. This is an internal 14 bit register that holds the number of MIDI beats (1 beat= six MIDI clocks) since the start of the song. l is the LSB, m the MSB.              |
|11110011     |   0sssssss            |    Song Select.  The Song Select specifies which sequence or song is to be played.                                                                                                             |
|11110100     |                       |    Undefined. (Reserved)                                                                                                                                                                       |
|11110101     |                       |    Undefined. (Reserved)                                                                                                                                                                       |
|11110110     |                       |    Tune Request. Upon receiving a Tune Request, all analog synthesizers should tune their oscillators.                                                                                         |
|11110111     |                       |    End of Exclusive. Used to terminate a System Exclusive dump (see above). |



**System Real-Time Messages**

| Status D7..D0 | Data Bytes           |Description     |
| ------------- | ---------------------|----------------|
|11111000   |          | Timing Clock. Sent 24 times per quarter note when synchronization is required (see text).         |
|11111001   |          | Undefined. (Reserved)                                                                             |
|11111010   |          | Start. Start the current sequence playing. (This message will be followed with Timing Clocks).    |
|11111011   |          | Continue. Continue at the point the sequence was Stopped.                                         |
|11111100   |          | Stop. Stop the current sequence.                                                                  |
|11111101   |          | Undefined. (Reserved)                                                                             |
|11111110   |          | Active Sensing. This message is intended to be sent repeatedly to tell the receiver that a connection is alive. Use of this message is optional. When initially received, the receiver will expect to receive another Active Sensing message each 300ms (max), and if it does not then it will assume that the connection has been terminated. At termination, the receiver will turn off all voices and return to normal (non- active sensing) operation. |
|11111111   |          | Reset. Reset all receivers in the system to power-up status. This should be used sparingly, preferably under manual control. In particular, it should not be sent on power-up.|

