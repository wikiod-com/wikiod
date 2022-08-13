---
title: "Get Started With Bluetooth LE on Windows"
slug: "get-started-with-bluetooth-le-on-windows"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

**Documentation**
- [Advertisement][1] - A representation of a Bluetooth LE advertisement payload.
- [Advertisement Publisher][2] - Manages the sending of Bluetooth LE advertisements.
- [Advertisement Watcher][3] - Manages the watching of Bluetooth LE advertisements.

**Notes**
 - Windows 10 can only act in central mode, so it can only connect to devices that support peripheral mode. Due to this, two Windows 10 devices cannot connect over Bluetooth LE.
 - Windows 10 must be paired with a Bluetooth LE device in order to connect to it.


  [1]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.devices.bluetooth.advertisement.bluetoothleadvertisement.aspx
  [2]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.devices.bluetooth.advertisement.bluetoothleadvertisementpublisher.aspx
  [3]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.devices.bluetooth.advertisement.bluetoothleadvertisementwatcher.aspx

## Initial Setup
To use any Bluetooth functionality on a Universal Windows Platform app, you must check the `Bluetooth` capability in the `Package.appxmanifest`.

1. Open `Package.appxmanifest`
2. Go to the `Capabilities` tab
3. Find `Bluetooth` on the left and check the box next to it

## Create a Bluetooth LE Advertisement
This example shows how to advertise a custom payload from a Windows 10 device in the foreground. The payload uses a made up company (identified as 0xFFFE) and advertises the string `Hello World` in the advertisement.

    BluetoothLEAdvertisementPublisher publisher = new BluetoothLEAdvertisementPublisher();

    // Add custom data to the advertisement
    var manufacturerData = new BluetoothLEManufacturerData();
    manufacturerData.CompanyId = 0xFFFE;

    var writer = new DataWriter();
    writer.WriteString("Hello World");

    // Make sure that the buffer length can fit within an advertisement payload (~20 bytes). 
    // Otherwise you will get an exception.
    manufacturerData.Data = writer.DetachBuffer();

    // Add the manufacturer data to the advertisement publisher:
    publisher.Advertisement.ManufacturerData.Add(manufacturerData);

    publisher.Start();

> Note: This is only for advertising in the foreground (while the app is open).

## Listen for a Bluetooth LE Advertisement
**General Listening**

This example shows how to listen for a specific advertisement.

    BluetoothLEAdvertisementWatcher watcher = new BluetoothLEAdvertisementWatcher();

    // Use active listening if you want to receive Scan Response packets as well
    // this will have a greater power cost.
    watcher.ScanningMode = BluetoothLEScanningMode.Active;

    // Register a listener, this will be called whenever the watcher sees an advertisement. 
    watcher.Received += OnAdvertisementReceived;

    watcher.Start();

**Advertisement Filter: Listening for a Specific Advertisement**

Sometimes you want to listen for a specific advertisement. In this case, listen for an advertisement containing a payload with a made up company (identified as 0xFFFE) and containing the string Hello World in the advertisement. This can be paired with the *Create a Bluetooth LE Advertisement* example to have one Windows machine advertising and another listening.

> Note: Be sure to set this advertisement filter before you start your watcher!


    var manufacturerData = new BluetoothLEManufacturerData();
    manufacturerData.CompanyId = 0xFFFE;

    // Make sure that the buffer length can fit within an advertisement payload (~20 bytes). 
    // Otherwise you will get an exception.
    var writer = new DataWriter();
    writer.WriteString("Hello World");
    manufacturerData.Data = writer.DetachBuffer();

    watcher.AdvertisementFilter.Advertisement.ManufacturerData.Add(manufacturerData);

**Signal Filter: Listening for Proximal Advertisements**

Sometimes you only want to trigger your watcher when the device advertising has come in range. You can define your own range, just note that normal values are between 0 and -128.

    // Set the in-range threshold to -70dBm. This means advertisements with RSSI >= -70dBm 
    // will start to be considered "in-range" (callbacks will start in this range).
    watcher.SignalStrengthFilter.InRangeThresholdInDBm = -70;
    
    // Set the out-of-range threshold to -75dBm (give some buffer). Used in conjunction 
    // with OutOfRangeTimeout to determine when an advertisement is no longer 
    // considered "in-range".
    watcher.SignalStrengthFilter.OutOfRangeThresholdInDBm = -75;

    // Set the out-of-range timeout to be 2 seconds. Used in conjunction with 
    // OutOfRangeThresholdInDBm to determine when an advertisement is no longer 
    // considered "in-range"
    watcher.SignalStrengthFilter.OutOfRangeTimeout = TimeSpan.FromMilliseconds(2000);

**Callbacks**

    watcher.Received += OnAdvertisementReceived;
    watcher.Stopped += OnAdvertisementWatcherStopped;

    private async void OnAdvertisementReceived(BluetoothLEAdvertisementWatcher watcher, BluetoothLEAdvertisementReceivedEventArgs eventArgs)
    {
        // Do whatever you want with the advertisement

        // The received signal strength indicator (RSSI)
        Int16 rssi = eventArgs.RawSignalStrengthInDBm;
    }


    private async void OnAdvertisementWatcherStopped(BluetoothLEAdvertisementWatcher watcher, BluetoothLEAdvertisementWatcherStoppedEventArgs eventArgs)
    {
        // Watcher was stopped
    }

> Note: This is only for listening in the foreground.



## Judging Distance Based on RSSI from a Bluetooth LE Advertisement
When your Bluetooth LE Watcher's callback is triggered, the eventArgs include an RSSI value telling you the received signal strength (how strong the 

    private async void OnAdvertisementReceived(BluetoothLEAdvertisementWatcher watcher, BluetoothLEAdvertisementReceivedEventArgs eventArgs)
    {
        // The received signal strength indicator (RSSI)
        Int16 rssi = eventArgs.RawSignalStrengthInDBm;
    }

This can be roughly translated into distance, but should not be used to measure true distances as each individual radio is different. Different environmental factors can make distance difficult to gauge (such as walls, cases around the radio, or even air humidity).

An alternative to judging pure distance is to define "buckets". Radios tend to report 0 to -50 DBm when they are very close, -50 to -90 when they are a medium distance away, and below -90 when they are far away. Trial and error is best to determine what you want these buckets to be for you application.

