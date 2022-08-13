---
title: "Scan & connect with BLE peripherals (Bluetooth Smart)"
slug: "scan--connect-with-ble-peripherals-bluetooth-smart"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This section describes how central (iOS app) can scan available BLE peripherals and connect with one we are interested in.  

## Scan all available peripherals

**Swift:**

    centralManager.scanForPeripherals(withServices: nil, options: nil)

**Objective C:**

    [centralManager scanForPeripheralsWithServices:nil options:nil];

## Scan available peripherals only with interested services
**Swift:**

    let services = [CBUUID(string: SERVICE1_UUID), CBUUID(string: SERVICE2_UUID)]
    centralManager.scanForPeripherals(withServices: services, options: nil)

**Objective C:**

    NSArray *services = @[[CBUUID UUIDWithString:SERVICE1_UUID], [CBUUID UUIDWithString:SERVICE2_UUID]];
    [centralManager scanForPeripheralsWithServices:services options:nil];

