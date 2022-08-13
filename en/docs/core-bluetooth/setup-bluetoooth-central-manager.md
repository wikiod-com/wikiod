---
title: "Setup bluetoooth central manager"
slug: "setup-bluetoooth-central-manager"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

CBCentralManager acts as a manager class central side app generally iOS app. which is responsible for bridging communication between peripherals and central. 

## Declare and instantiate CBCentralManager

Swift:

    var centralManager:CBCentralManager!
    
    centralManager = CBCentralManager(delegate: self, queue: nil)


Objective C:

    @property (nonatomic, strong) CBCentralManager *centralManager;

    centralManager = [[CBCentralManager alloc] initWithDelegate:self queue:nil];

## Declare and instantiate CBCentralManager with options

**Swift:**

    var centralManager:CBCentralManager!
    
    centralManager = CBCentralManager(delegate: self, queue: nil, options: [CBCentralManagerOptionRestoreIdentifierKey : "com.companyname.appname.central"])

**Objective C:**

    @property (nonatomic, strong) CBCentralManager *centralManager;
    
    self.centralManager = [[CBCentralManager alloc] initWithDelegate:self queue:nil options:@{CBCentralManagerOptionRestoreIdentifierKey : @"com.companyname.appname.central"}];



