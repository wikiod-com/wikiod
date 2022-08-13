---
title: "Hello World"
slug: "hello-world"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Editing index.ios.js or index.android.js 
Open `index.ios.js` or `index.android.js` and delete everything between the `<View> </View>`. After that, write `<Text> Hello World! </Text>` and run the emulator. 

You should see `Hello World!` written on the screen!

Congrats! You've successfully written your first Hello World!

## Hello world!
    import React, { Component } from 'react';
    import { AppRegistry, Text } from 'react-native';
    
    class HelloWorldApp extends Component {
      render() {
        return (
          <Text>Hello world!</Text>
        );
      }
    }
    
    AppRegistry.registerComponent('HelloWorldApp', () => HelloWorldApp);

