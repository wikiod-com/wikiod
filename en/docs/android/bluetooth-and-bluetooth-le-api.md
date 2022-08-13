---
title: "Bluetooth and Bluetooth LE API"
slug: "bluetooth-and-bluetooth-le-api"
draft: false
images: []
weight: 9878
type: docs
toc: true
---

Bluetooth Classic is available from Android 2.0 (API level 5) and above. Bluetooth LE is available from Android 4.3 (API level 18) and above.

## Permissions
Add this permission to the manifest file to use Bluetooth features in your application:

    <uses-permission android:name="android.permission.BLUETOOTH" />

If you need to initiate device discovery or manipulate Bluetooth settings, you also need to add this permission:

    <uses-permission android:name="android.permission.BLUETOOTH_ADMIN" />


Targetting Android API level 23 and above, will require location access:

    <uses-permission android:name="android.permission.ACCESS_COARSE_LOCATION" />
    <!-- OR -->
    <uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />
    

\* Also see the [Permissions][1] topic for more details on how to use permissions appropriately.

 

  [1]: https://www.wikiod.com/android/runtime-permissions-in-api-23-+





## Check if bluetooth is enabled
    private static final int REQUEST_ENABLE_BT = 1; // Unique request code
    BluetoothAdapter mBluetoothAdapter;

    // ...

    if (!mBluetoothAdapter.isEnabled()) {
        Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
        startActivityForResult(enableBtIntent, REQUEST_ENABLE_BT);
    }

    // ...

    @Override
    protected void onActivityResult(final int requestCode, final int resultCode, final Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        if (requestCode == REQUEST_ENABLE_BT) {
            if (resultCode == RESULT_OK) {
                // Bluetooth was enabled
            } else if (resultCode == RESULT_CANCELED) {
                // Bluetooth was not enabled
            }
        }
    }

## Make device discoverable
    private static final int REQUEST_DISCOVERABLE_BT = 2; // Unique request code
    private static final int DISCOVERABLE_DURATION = 120; // Discoverable duration time in seconds
                                                          // 0 means always discoverable
                                                          // maximum value is 3600

    // ...

    Intent discoverableIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_DISCOVERABLE);
    discoverableIntent.putExtra(BluetoothAdapter.EXTRA_DISCOVERABLE_DURATION, DISCOVERABLE_DURATION);
    startActivityForResult(discoverableIntent, REQUEST_DISCOVERABLE_BT);

    // ...

    @Override
    protected void onActivityResult(final int requestCode, final int resultCode, final Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        if (requestCode == REQUEST_DISCOVERABLE_BT) {
            if (resultCode == RESULT_OK) {
                // Device is discoverable
            } else if (resultCode == RESULT_CANCELED) {
                // Device is not discoverable
            }
        }
    }

## Find nearby Bluetooth Low Energy devices
The BluetoothLE API was introduced in API 18. However, the way of scanning devices has changed in API 21. The searching of devices must start with defining the [service UUID](https://learn.adafruit.com/introduction-to-bluetooth-low-energy/gatt#services) that is to be scanned (either officailly adopted 16-bit UUID's or proprietary ones). This example illustrates, how to make an API independent way of searching for BLE devices:

 1. Create bluetooth device model:


    public class BTDevice {
        String address;
        String name;
    
        public String getAddress() {
            return address;
        }
    
        public void setAddress(String address) {
            this.address = address;
        }
    
        public String getName() {
            return name;
        }
    
        public void setName(String name) {
            this.name = name;
        }
    }

 2. Define Bluetooth Scanning interface:


    public interface ScanningAdapter {
    
        void startScanning(String name, String[] uuids);
        void stopScanning();
        List<BTDevice> getFoundDeviceList();
    }

 3. Create scanning factory class:


    public class BluetoothScanningFactory implements ScanningAdapter {
    
        private ScanningAdapter mScanningAdapter;
    
        public BluetoothScanningFactory() {
            if (isNewerAPI()) {
                mScanningAdapter = new LollipopBluetoothLEScanAdapter();
            } else {
                mScanningAdapter = new JellyBeanBluetoothLEScanAdapter();
            }
        }
    
        private boolean isNewerAPI() {
            return Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP;
        }
    
        @Override
        public void startScanning(String[] uuids) {
            mScanningAdapter.startScanning(uuids);
        }
    
        @Override
        public void stopScanning() {
            mScanningAdapter.stopScanning();
        }
    
        @Override
        public List<BTDevice> getFoundDeviceList() {
            return mScanningAdapter.getFoundDeviceList();
        }
    }

 4. Create factory implementation for each API:

   **API 18:**

    import android.annotation.TargetApi;
    import android.bluetooth.BluetoothAdapter;
    import android.bluetooth.BluetoothDevice;
    import android.os.Build;
    import android.os.Parcelable;
    import android.util.Log;
    
    import bluetooth.model.BTDevice;
    
    import java.util.ArrayList;
    import java.util.List;
    import java.util.UUID;

    @TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR2)
    public class JellyBeanBluetoothLEScanAdapter implements ScanningAdapter{
        BluetoothAdapter bluetoothAdapter;
        ScanCallback mCallback;
    
        List<BTDevice> mBluetoothDeviceList;
    
        public JellyBeanBluetoothLEScanAdapter() {
            bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
            mCallback = new ScanCallback();
            mBluetoothDeviceList = new ArrayList<>();
        }
    
        @Override
        public void startScanning(String[] uuids) {
            if (uuids == null || uuids.length == 0) {
                return;
            }
            UUID[] uuidList = createUUIDList(uuids);
            bluetoothAdapter.startLeScan(uuidList, mCallback);
        }
    
        private UUID[] createUUIDList(String[] uuids) {
            UUID[] uuidList = new UUID[uuids.length];
            for (int i = 0 ; i < uuids.length ; ++i) {
                String uuid = uuids[i];
                if (uuid == null) {
                    continue;
                }
                uuidList[i] = UUID.fromString(uuid);
            }
            return uuidList;
        }
    
        @Override
        public void stopScanning() {
            bluetoothAdapter.stopLeScan(mCallback);
        }
    
        @Override
        public List<BTDevice> getFoundDeviceList() {
            return mBluetoothDeviceList;
        }
    
        private class ScanCallback implements BluetoothAdapter.LeScanCallback {
    
            @Override
            public void onLeScan(BluetoothDevice device, int rssi, byte[] scanRecord) {
                if (isAlreadyAdded(device)) {
                    return;
                }
                BTDevice btDevice = new BTDevice();
                btDevice.setName(new String(device.getName()));
                btDevice.setAddress(device.getAddress());
                mBluetoothDeviceList.add(btDevice);
                Log.d("Bluetooth discovery", device.getName() + " " + device.getAddress());
                Parcelable[] uuids = device.getUuids();
                String uuid = "";
                if (uuids != null) {
                    for (Parcelable ep : uuids) {
                        uuid += ep + " ";
                    }
                    Log.d("Bluetooth discovery", device.getName() + " " + device.getAddress() + " " + uuid);
                }
            }
    
            private boolean isAlreadyAdded(BluetoothDevice bluetoothDevice) {
                for (BTDevice device : mBluetoothDeviceList) {
                    String alreadyAddedDeviceMACAddress = device.getAddress();
                    String newDeviceMACAddress = bluetoothDevice.getAddress();
                    if (alreadyAddedDeviceMACAddress.equals(newDeviceMACAddress)) {
                        return true;
                    }
                }
                return false;
            }
        }
    }


   **API 21:**

    import android.annotation.TargetApi;
    import android.bluetooth.BluetoothAdapter;
    import android.bluetooth.le.BluetoothLeScanner;
    import android.bluetooth.le.ScanFilter;
    import android.bluetooth.le.ScanResult;
    import android.bluetooth.le.ScanSettings;
    import android.os.Build;
    import android.os.ParcelUuid;
    
    import bluetooth.model.BTDevice;
    
    import java.util.ArrayList;
    import java.util.List;

    @TargetApi(Build.VERSION_CODES.LOLLIPOP)
    public class LollipopBluetoothLEScanAdapter implements ScanningAdapter {
        BluetoothLeScanner bluetoothLeScanner;
        ScanCallback mCallback;
    
        List<BTDevice> mBluetoothDeviceList;
    
        public LollipopBluetoothLEScanAdapter() {
            bluetoothLeScanner = BluetoothAdapter.getDefaultAdapter().getBluetoothLeScanner();
            mCallback = new ScanCallback();
            mBluetoothDeviceList = new ArrayList<>();
        }
    
        @Override
        public void startScanning(String[] uuids) {
            if (uuids == null || uuids.length == 0) {
                return;
            }
            List<ScanFilter> filterList = createScanFilterList(uuids);
            ScanSettings scanSettings = createScanSettings();
            bluetoothLeScanner.startScan(filterList, scanSettings, mCallback);
        }
    
        private List<ScanFilter> createScanFilterList(String[] uuids) {
            List<ScanFilter> filterList = new ArrayList<>();
            for (String uuid : uuids) {
                ScanFilter filter = new ScanFilter.Builder()
                        .setServiceUuid(ParcelUuid.fromString(uuid))
                        .build();
                filterList.add(filter);
            };
            return filterList;
        }
    
        private ScanSettings createScanSettings() {
            ScanSettings settings = new ScanSettings.Builder()
                    .setScanMode(ScanSettings.SCAN_MODE_BALANCED)
                    .build();
            return settings;
        }
    
        @Override
        public void stopScanning() {
            bluetoothLeScanner.stopScan(mCallback);
        }
    
        @Override
        public List<BTDevice> getFoundDeviceList() {
            return mBluetoothDeviceList;
        }
    
        public class ScanCallback extends android.bluetooth.le.ScanCallback {
    
            @Override
            public void onScanResult(int callbackType, ScanResult result) {
                super.onScanResult(callbackType, result);
                if (result == null) {
                    return;
                }
                BTDevice device = new BTDevice();
                device.setAddress(result.getDevice().getAddress());
                device.setName(new StringBuffer(result.getScanRecord().getDeviceName()).toString());
                if (device == null || device.getAddress() == null) {
                    return;
                }
                if (isAlreadyAdded(device)) {
                    return;
                }
                mBluetoothDeviceList.add(device);
            }
    
            private boolean isAlreadyAdded(BTDevice bluetoothDevice) {
                for (BTDevice device : mBluetoothDeviceList) {
                    String alreadyAddedDeviceMACAddress = device.getAddress();
                    String newDeviceMACAddress = bluetoothDevice.getAddress();
                    if (alreadyAddedDeviceMACAddress.equals(newDeviceMACAddress)) {
                        return true;
                    }
                }
                return false;
            }
        }
    }

    

 5. Get found device list by calling:

        scanningFactory.startScanning({uuidlist});
        
        wait few seconds...

        List<BTDevice> bluetoothDeviceList = scanningFactory.getFoundDeviceList();


## Find nearby bluetooth devices
Declare a `BluetoothAdapter` first.

    BluetoothAdapter mBluetoothAdapter;

Now create a `BroadcastReceiver` for `ACTION_FOUND`

    private final BroadcastReceiver mReceiver = new BroadcastReceiver() {
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();
    
        //Device found                
        if (BluetoothDevice.ACTION_FOUND.equals(action)) 
        {
            // Get the BluetoothDevice object from the Intent
            BluetoothDevice device = intent.getParcelableExtra(BluetoothDevice.EXTRA_DEVICE);
            // Add the name and address to an array adapter to show in a list
            mArrayAdapter.add(device.getName() + "\n" + device.getAddress());
        }
      }
    };

Register the `BroadcastReceiver`

    IntentFilter filter = new IntentFilter(BluetoothDevice.ACTION_FOUND);
    registerReceiver(mReceiver, filter);

Then start discovering the nearby bluetooth devices by calling `startDiscovery`

    mBluetoothAdapter.startDiscovery(); 
    
Don't forget to unregister the `BroadcastReceiver` inside `onDestroy`

    unregisterReceiver(mReceiver);

## Connect to Bluetooth device
After you obtained BluetoothDevice, you can communicate with it. 
This kind of communication performed by using socket input\output streams:
 
Those are the basic steps for Bluetooth communication establishment:

**1) Initialize socket:**

     private BluetoothSocket _socket;
     //...
     public InitializeSocket(BluetoothDevice device){
        try {
            _socket = device.createRfcommSocketToServiceRecord(<Your app UDID>);
        } catch (IOException e) {
            //Error
        }
      }
    

**2) Connect to socket:**

    try {
        _socket.connect();
    } catch (IOException connEx) {
        try {
            _socket.close();
        } catch (IOException closeException) {
            //Error
        }
    }
    
    if (_socket != null && _socket.isConnected()) {
        //Socket is connected, now we can obtain our IO streams
    }
            
    
**3) Obtaining socket Input\Output streams**

    private InputStream _inStream;
    private OutputStream _outStream;
    //....
    try {
        _inStream = _socket.getInputStream();
        _outStream =  _socket.getOutputStream();
    } catch (IOException e) {
       //Error
    }
        

*Input stream* - Used as incoming data channel (receive data from connected device)

*Output stream* - Used as outgoing data channel (send data to connected device)



**After finishing 3rd step, we can receive and send data between both devices using previously initialized streams:**

*1) Receiving data (reading from socket input stream)*


    byte[] buffer = new byte[1024];  // buffer (our data)
    int bytesCount; // amount of read bytes
    
    while (true) {
        try {
            //reading data from input stream
            bytesCount = _inStream.read(buffer);
            if(buffer != null && bytesCount > 0)
            {
                //Parse received bytes
            }
        } catch (IOException e) {
            //Error
        }
    }

*2) Sending data (Writing to output stream)*

    public void write(byte[] bytes) {
        try {
            _outStream.write(bytes);
        } catch (IOException e) {
            //Error
        }
    }
    
* Of course, connection, reading and writing functionality should be done in a dedicated thread.
* Sockets and Stream objects need to be
    





