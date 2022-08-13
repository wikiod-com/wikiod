---
title: "Time Management"
slug: "time-management"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Syntax
- unsigned long millis()
- unsigned long micros()
- void delay(unsigned long milliseconds)
- void delayMicroseconds(unsigned long microseconds)

- See [the elapsedMillis header][1] for constructors and operators of that class. In short:

  - elapsedMillis elapsedMillisObject; *creates an object to keep track of time since it was created or since some other explicitly set point in time*
  - elapsedMillisObject = 0; *reset the time tracked by the object to "since now"*
  - unsigned long deltaT = elapsedMillisObject; *lets us look at the tracked time*
  - elapsedMillisObject += and -= *these work as expected*


  [1]: https://github.com/pfeerick/elapsedMillis/blob/master/elapsedMillis.h "elapsedMillis header"

Blocking vs. non-blocking code
------------------------------

For very simple sketches, writing blocking code using `delay()` and `delayMicroseconds()` can be appropriate. When things get more complex, using these functions can have some drawbacks. Some of these are:

- Wasting CPU time: More complex sketches might need the CPU for something else while waiting for an LED blinking period to end.
- unexpected delays: when `delay()` is called in subroutines that are not obviously called, for example in libraries you include.
- missing events that happen during the delay and are not handled by an interrupt handler, for example polled button presses: A button might be pressed for 100 ms, but this might be shadowed by a `delay(500)`. 

Implementation details
----------------------

`millis()` usually relies on a hardware timer that runs at a speed that's much higher than 1 kHz. When `millis()` is called, the implementation returns some value, but you don't know how old that actually is. It's possible that the "current" millisecond just started, or that it will end right after that function call. That means that, when calculating the difference between two results from `millis()`, you can be off by anything between almost zero and almost one millisecond. Use `micros()` if higher precision is needed.

Looking into the source code of `elapsedMillis` reveals that it indeed uses `millis()` internally to compare two points in time, so it suffers from this effect as well. Again, there's the alternative `elapsedMicros` for higher precision, from the same library.

## Non-blocking blinky with millis()
This is very close to [an example from the arduino docs][1]:

    // set constants for blinking the built-in LED at 1 Hz
    #define OUTPIN LED_BUILTIN
    #define PERIOD 500  // this is in milliseconds
    
    int ledState = LOW;
    
    // millis() returns an unsigned long so we'll use that to keep track of time
    unsigned long lastTime = 0;
    
    void setup() {
      // set the digital pin as output:
      pinMode(OUTPIN, OUTPUT);
    }
    
    void loop() {
      unsigned long now = millis();
      if (now - lastTime >= PERIOD) // this will be true every PERIOD milliseconds
      {
        lastTime = now;
        if (ledState == LOW)
        {
          ledState = HIGH;
        }
        else
        {
          ledState = LOW;
        }
        digitalWrite(OUTPIN, ledState);
      }

      // now there's lots of time to do other stuff here
    }

Using `millis()` in this way - to time operations in a non-blocking way - is something that is needed quite frequently, so consider using the `elapsedMillis` library for this.

  [1]: https://www.arduino.cc/en/Tutorial/BlinkWithoutDelay

## Non-blocking blinky with the elapsedMillis library (and class)
The [elapsedMillis library][1] provides a class with the same name that keeps track of the time that passed since it was created or set to a certain value:

    #include <elapsedMillis.h>
    
    #define OUTPIN LED_BUILTIN
    #define PERIOD 500
    
    elapsedMillis ledTime;
    
    bool ledState = false;
    
    void setup() 
    {                
      // initialize the digital pin as an output.
      pinMode(OUTPIN, OUTPUT);     
    }
    
    void loop()
    {
        if (ledTime >= PERIOD) 
        {                
            ledState = !ledState;
            digitalWrite(OUTPIN, ledState);
            ledTime = 0;
        }
        // do other stuff here
    }

You can see in the example that the `ledTime` object is assigned zero when the LED pin was toggled. This might not be surprising at first glance, but it has an effect if more time-consuming things are happening:

Consider a situation where the comparison between `ledTime` and `PERIOD` is done after 750 milliseconds. Then setting `ledTime` to zero means that all following toggle operations will be 250 ms "late". If, in contrast, `PERIOD` was subtracted from `ledTime`, the LED would see one short period and then continue blinking as if nothing happened.


  [1]: http://playground.arduino.cc/Code/ElapsedMillis

## blocking blinky with delay()
One of the most straight forward way of making an LED blink is: turn it on, wait a bit, turn it off, wait again, and repeat endlessly:

    // set constants for blinking the built-in LED at 1 Hz
    #define OUTPIN LED_BUILTIN
    #define PERIOD 500
    
    void setup()
    {
      pinMode(OUTPIN, OUTPUT);      // sets the digital pin as output
    }
    
    void loop()
    {
      digitalWrite(OUTPIN, HIGH);   // sets the pin on
      delayMicroseconds(PERIOD);        // pauses for 500 miliseconds      
      digitalWrite(OUTPIN, LOW);    // sets the pin off
      delayMicroseconds(PERIOD);        // pauses for 500 milliseconds

      // doing other time-consuming stuff here will skew the blinking
    }

However, waiting as done in the example above wastes CPU cycles, because it just sits there in a loop waiting for a certain point in time to go past. That's what the non-blocking ways, using `millis()` or `elapsedMillis`, do better - in the sense that they don't burn as much of the hardware's capabilities.

## Measure how long something took, using elapsedMillis and elapsedMicros
    #include <elapsedMillis.h>
    
    void setup() {
      Serial.begin(115200);
      elapsedMillis msTimer;
      elapsedMicros usTimer;
    
      long int dt = 500;
      delay(dt);
    
      long int us = usTimer;
      long int ms = msTimer;
    
      Serial.print("delay(");Serial.print(dt);Serial.println(") took");
      Serial.print(us);Serial.println(" us, or");
      Serial.print(ms);Serial.println(" ms");
    }
    
    void loop() {
    }

In this example, an `elapsedMillis` object and an `elapsedMicros` object are used to measure how long something took, by creating them just before the expression we want to time is executed, and getting their values afterwards. They will show slightly different results, but the millisecond result won't be off by more than one millisecond.

## More than 1 task without delay()
If you have more than 1 task to execute repeatedly in different intervals, use this example as a starting point:

    unsigned long intervals[] = {250,2000}; //this defines the interval for each task in milliseconds
    unsigned long last[] = {0,0};           //this records the last executed time for each task
    
    void setup() {
      pinMode(LED_BUILTIN, OUTPUT); //set the built-it led pin as output
      Serial.begin(115200);         //initialize serial
    }
    
    void loop() {
      unsigned long now = millis();
      if(now-last[0]>=intervals[0]){ last[0]=now; firstTask(); }
      if(now-last[1]>=intervals[1]){ last[1]=now; secondTask(); }
      
      //do other things here
    }
    
    void firstTask(){
      //let's toggle the built-in led
      digitalWrite(LED_BUILTIN, digitalRead(LED_BUILTIN)?0:1);
    }
    
    void secondTask(){
      //say hello
      Serial.println("hello from secondTask()");
    }

To add another task to execute every 15 seconds, extend the variables `intervals` and `last`:

    unsigned long intervals[] = {250,2000,15000};
    unsigned long last[] = {0,0,0};

Then add an `if` statement to execute the new task.
In this example, I named it `thirdTask`.

    if(now-last[2]>=intervals[2]){ last[2]=now; thirdTask(); }

Finally declare the function:

    void thirdTask(){
      //your code here
    }


