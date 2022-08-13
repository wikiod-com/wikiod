---
title: "iBeacon"
slug: "ibeacon"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

## Parameters
| Parameters | Details |
| ------ | ------ |
| manager   | CLLocationManager reference 
|region     | CLRegion   could be circular region (geofence or beacon region)       
|beacons    | Array of  CLBeacon contains all ranged beacons

 

Beacons are IOT objects. We are focusing on those which conform to iBeacon protocol a Apple standard. Each beacon is a one way device which transmits 3 things

1. UUID   
2. Major
3. Minor

We can scan iBeacons by setting up our CLLocation manager object to scan for beacons for particular UUID. All beacons with the given UUID will be scanned.

CLLocation manager also gives call on enter and exit of beacon region. 
   

## iBeacon Basic Operation
1. Setup monitoring beacons



    func initiateRegion(ref:BeaconHandler){
        let uuid: NSUUID = NSUUID(UUIDString: "<UUID>")
        let beacon = CLBeaconRegion(proximityUUID: uuid, identifier: "")
        locationManager?.requestAlwaysAuthorization()    //cllocation manager obj.
        beacon?.notifyOnEntry = true
        beacon?.notifyOnExit = true
        beacon?.notifyEntryStateOnDisplay = true
        locationManager?.startMonitoringForRegion(beacon!)
        locationManager?.delegate = self;
        // Check if beacon monitoring is available for this device
        if (!CLLocationManager.isMonitoringAvailableForClass(CLBeaconRegion)) {
            print("error")
        }
        locationManager!.startRangingBeaconsInRegion(self.beacon!)
    }

2. Location manager enter and exit region


    func locationManager(manager: CLLocationManager, didEnterRegion region: CLRegion) {
        if(region.isKindOfClass(CLBeaconRegion)) {
            locationManager!.startRangingBeaconsInRegion(self.beacon!)
        }
    }
        
    func locationManager(manager: CLLocationManager, didExitRegion region: CLRegion) {
        if(region.isKindOfClass(CLBeaconRegion)) {
            locationManager!.stopRangingBeaconsInRegion(self.beacon!)
        }
    }

3. Location manager range beacon


    func locationManager(manager: CLLocationManager, didRangeBeacons beacons: [CLBeacon], inRegion region: CLBeaconRegion) {
        print(beacons.first.major)
    }

## Scanning specific Beacons
     beacon = CLBeaconRegion(proximityUUID: <#NSUUID#>, major: <#CLBeaconMajorValue#>, identifier: <#String#>) // listening to all beacons with given UUID and major value
     beacon =    CLBeaconRegion(proximityUUID: <##NSUUID#>, major: <##CLBeaconMajorValue#>, minor: <##CLBeaconMinorValue#>, identifier: <##String#>) // listening to all beacons with given UUID and major and minor value


## Ranging iBeacons


