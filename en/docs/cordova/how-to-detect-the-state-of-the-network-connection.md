---
title: "How to detect the state of the network connection"
slug: "how-to-detect-the-state-of-the-network-connection"
draft: false
images: []
weight: 9828
type: docs
toc: true
---

## Using the cordova-plugin-network-information plugin
Detecting the current state of the network connection and responding to any changes that might occur, can be done by using one of several plugins. This example is about the [cordova-plugin-network-information][1] plugin.

Add the plugin to the project:

    cordova plugin add cordova-plugin-network-information

After the Cordova [deviceready event][2] a connection object is available through `navigator.connection`. The `type` property contains the current network state:

    document.addEventListener("deviceready", function() {
        var networkState = navigator.connection.type;
    }, false);

`networkState` now contains one of the following constants:

    Connection.UNKNOWN  //  Unknown connection
    Connection.ETHERNET //  Ethernet connection
    Connection.WIFI     //  WiFi connection
    Connection.CELL_2G  //  Cell 2G connection
    Connection.CELL_3G  //  Cell 3G connection
    Connection.CELL_4G  //  Cell 4G connection
    Connection.CELL     //  Cell generic connection
    Connection.NONE     //  No network connection

Detecting a change in network connection can be done by hooking a function to either the `online` or the `offline` event:

    document.addEventListener("online", function() {
        // device went online
        var networkState = navigator.connection.type; // Get new network state
        ...
    }, false);

    document.addEventListener("offline", function() {
        // device went offline
        var networkState = navigator.connection.type; // Get new network state
        ...
    }, false);

  [1]: https://cordova.apache.org/docs/en/latest/reference/cordova-plugin-network-information/index.html
  [2]: https://cordova.apache.org/docs/en/latest/cordova/events/events.html#deviceready

