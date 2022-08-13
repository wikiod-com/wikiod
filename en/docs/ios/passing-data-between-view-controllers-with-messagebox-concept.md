---
title: "Passing Data between View Controllers (with MessageBox-Concept)"
slug: "passing-data-between-view-controllers-with-messagebox-concept"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

MessageBox is a simple concept for decoupling entities.

For example entity A can place a message that entity B can read whenever suitable.

A view controller would like to talk to another view controller, but you don't want to create a strong or weak relationship.

## Simple Example Usage
[![enter image description here][1]][1]

    let messageBox:MessageBox = MessageBox()
    
    // set
    messageBox.setObject("TestObject1", forKey:"TestKey1")
    
    // get
    // but don't remove it, keep it stored, so that it can still be retrieved later
    let someObject:String = messageBox.getObject(forKey:"TestKey1", removeIfFound:false)
    
    // get
    // and remove it
    let someObject:String = messageBox.getObject(forKey:"TestKey1", removeIfFound:true)


  [1]: https://i.stack.imgur.com/KM9LD.png

