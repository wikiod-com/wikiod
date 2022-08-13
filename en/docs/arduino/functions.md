---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9987
type: docs
toc: true
---


Other than in ordinary C / C++ , the Arduino IDE allows to call a function before it is defined. 

In .cpp files, you have to define the function, or at least declare the function prototype before you can use it. <br>In an .ino file, the Arduino IDE creates such a prototype behind the scenes.

[Arduino - function declaration - official][1]


  [1]: https://www.arduino.cc/en/Reference/FunctionDeclaration

## Create simple function
    int squareNum (int a) {
        return a*a;
    }

**`int`** : return type

**`squareNum`** : function name

**`int a`** : parameter type and name

**`return a*a`** : return a value (same type as the return type defined at the beginning)

[![C - Arduino function anatomy][1]][1]


  [1]: http://i.stack.imgur.com/2GZal.png

## Call a function
If you have a function declared you can call it anywhere else in the code.
Here is an example of calling a function:

    void setup(){
      Serial.begin(9600);
    }
    
    void loop() {
      int i = 2;
    
      int k = squareNum(i); // k now contains 4
      Serial.println(k);
      delay(500);
    }

    int squareNum(int a) {
        return a*a;
    }

