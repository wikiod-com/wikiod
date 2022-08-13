---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Routing or navigation allows applications to between different screens. Its vital to a mobile app as it provides context to user about where they are, decouple user actions between screens and move between them, provide a state machine like model of the whole app.

## Navigator component
Navigator works for both IOS and android.

    import React, { Component } from 'react';
    import { Text, Navigator, TouchableHighlight } from 'react-native';

    export default class NavAllDay extends Component {
      render() {
        return (
          <Navigator
            initialRoute={{ title: 'Awesome Scene', index: 0 }}
            renderScene={(route, navigator) =>
              <Text>Hello {route.title}!</Text>
            }
            style={{padding: 100}}
          />
        );
      }
    }
Routes to `Navigator` are provided as objects. You also provide a `renderScene` function that renders the scene for each route object. `initialRoute` is used to specify the first route.

