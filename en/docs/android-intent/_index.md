---
title : android-intent Tutorial
slug : android-intent-tutorial
weight : 9985
draft : false
images : []
type : docs
---

**Types of Intents**

 1. **Explicit Intents**
 2. **Implicit Intents**

**Explicit intent**: going to be connected internal world of application, suppose you want to connect one activity to another activity, this can be done by explicit intent. Below is the code snippet demonstrating the connection between first and second activity:

  
    // Explicit Intent by specifying its class name
    Intent intent_activity = new Intent(FirstActivity.this, SecondActivity.class);
    
    // Starts TargetActivity
    startActivity(intent_activity);


**Implicit Intents**: these intents do not name a target and the field for target component name is left blank. Implicit intents are often used to activate components in other applications. For example: 

    Intent intent_message= new Intent(Intent.ACTION_SEND); 
    intent_message.setData(Uri.fromFile(fileToShare));
    startActivity(intent_message);  

