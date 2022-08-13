---
title: "Get Started with Bluetooth on the Web"
slug: "get-started-with-bluetooth-on-the-web"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

**Sources:**
- https://developers.google.com/web/updates/2015/07/interact-with-ble-devices-on-the-web
- https://googlechrome.github.io/samples/web-bluetooth/index.html

## Read Battery Level from a nearby Bluetooth device (readValue)
<!-- language-all: lang-js -->

    function onButtonClick() {
    
      navigator.bluetooth.requestDevice({filters: [{services: ['battery_service']}]})
      .then(device => {
        // Connecting to GATT Server...
        return device.gatt.connect();
      })
      .then(server => {
        // Getting Battery Service...
        return server.getPrimaryService('battery_service');
      })
      .then(service => {
        // Getting Battery Level Characteristic...
        return service.getCharacteristic('battery_level');
      })
      .then(characteristic => {
        // Reading Battery Level...
        return characteristic.readValue();
      })
      .then(value => {
        let batteryLevel = value.getUint8(0);
        console.log('> Battery Level is ' + batteryLevel + '%');
      })
      .catch(error => {
        console.log('Argh! ' + error);
      });
    }

## Reset energy expended from a nearby Bluetooth Device (writeValue)
<!-- language-all: lang-js -->

    function onButtonClick() {
        
      navigator.bluetooth.requestDevice({filters: [{services: ['heart_rate']}]})
      .then(device => {
        // Connecting to GATT Server...
        return device.gatt.connect();
      })
      .then(server => {
        // Getting Heart Rate Service...
        return server.getPrimaryService('heart_rate');
      })
      .then(service => {
        // Getting Heart Rate Control Point Characteristic...
        return service.getCharacteristic('heart_rate_control_point');
      })
      .then(characteristic => {    
        // Writing 1 is the signal to reset energy expended.
        let resetEnergyExpended = new Uint8Array([1]);
        return characteristic.writeValue(resetEnergyExpended);
      })
      .then(_ => {
        console.log('> Energy expended has been reset.');
      })
      .catch(error => {
        console.log('Argh! ' + error);
      });
    }

