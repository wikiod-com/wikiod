---
title: "Runtime Permissions in API-23 +"
slug: "runtime-permissions-in-api-23-+"
draft: false
images: []
weight: 9367
type: docs
toc: true
---

Android Marshmallow introduced [Runtime Permission][1] model. Permissions are categorized into two categories i.e. [Normal and Dangerous Permissions][2]. where [dangerous permissions][3] are now granted by the user at run time.

  [1]: https://developer.android.com/training/permissions/requesting.html
  [2]: https://developer.android.com/guide/topics/permissions/requesting.html#normal-dangerous
  [3]: https://developer.android.com/guide/topics/permissions/requesting.html#perm-groups

From sdk 23 Android requires runtime permissions for permissions on devices running Android 6.0 and higher, within what is classed as the Dangerous Permission Groups. Dangerous permission groups are one's that are considered to compromise the user's privacy and/or security.

The following is a list of Dangerous Permission Groups:

**Dangerous Permission Groups**

*Permission Group*    
CALENDAR    
CAMERA    
CONTACTS      
LOCATION      
MICROPHONE      
PHONE    
SENSORS    
SMS    
STORAGE    

Any permissions from these groups requires management of runtime permissions for devices on Android 6.0 and higher with a target sdk of 23 or higher.


**Normal Permissions**

The following is a list of normal permissions. These are not regarded as dangerous to the user's privacy or security and so do not require runtime permissions for sdk 23 and higher.

ACCESS_LOCATION_EXTRA_COMMANDS  
ACCESS_NETWORK_STATE  
ACCESS_NOTIFICATION_POLICY  
ACCESS_WIFI_STATE  
BLUETOOTH  
BLUETOOTH_ADMIN  
BROADCAST_STICKY  
CHANGE_NETWORK_STATE  
CHANGE_WIFI_MULTICAST_STATE  
CHANGE_WIFI_STATE  
DISABLE_KEYGUARD  
EXPAND_STATUS_BAR  
GET_PACKAGE_SIZE  
INSTALL_SHORTCUT  
INTERNET  
KILL_BACKGROUND_PROCESSES  
MODIFY_AUDIO_SETTINGS  
NFC  
READ_SYNC_SETTINGS  
READ_SYNC_STATS  
RECEIVE_BOOT_COMPLETED  
REORDER_TASKS  
REQUEST_IGNORE_BATTERY_OPTIMIZATIONS  
REQUEST_INSTALL_PACKAGES  
SET_ALARM  
SET_TIME_ZONE  
SET_WALLPAPER  
SET_WALLPAPER_HINTS  
TRANSMIT_IR  
UNINSTALL_SHORTCUT  
USE_FINGERPRINT  
VIBRATE  
WAKE_LOCK  
WRITE_SYNC_SETTINGS  


## Android 6.0 multiple permissions
This example shows how to check permissions at runtime in Android 6 and later.

    public static final int MULTIPLE_PERMISSIONS = 10; // code you want.

    String[] permissions = new String[] {
        Manifest.permission.WRITE_EXTERNAL_STORAGE,
        Manifest.permission.CAMERA,
        Manifest.permission.ACCESS_COARSE_LOCATION,
        Manifest.permission.ACCESS_FINE_LOCATION
    };
    
    @Override
    void onStart() {
        if (checkPermissions()){
            // permissions granted.    
        } else {
            // show dialog informing them that we lack certain permissions
        }
    }
    
    private boolean checkPermissions() {
        int result;
        List<String> listPermissionsNeeded = new ArrayList<>();
        for (String p:permissions) {
            result = ContextCompat.checkSelfPermission(getActivity(),p);
            if (result != PackageManager.PERMISSION_GRANTED) {
                listPermissionsNeeded.add(p);
            }
        }
        if (!listPermissionsNeeded.isEmpty()) {
            ActivityCompat.requestPermissions(this, listPermissionsNeeded.toArray(new String[listPermissionsNeeded.size()]), MULTIPLE_PERMISSIONS);
            return false;
        }
        return true;
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, String permissions[], int[] grantResults) {
        switch (requestCode) {
            case MULTIPLE_PERMISSIONS:{
                if(grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED){
                    // permissions granted.
                } else {
                    // no permissions granted.
                }
                return;
            }
        }
    }


## Multiple Runtime Permissions From Same Permission Groups
In the manifest we have fours dangerous runtime permissions from two groups.

    <!-- Required to read and write to shredPref file. -->
    <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE"/>
    <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE"/>
    
    <!-- Required to get location of device. -->
    <uses-permission android:name="android.permission.ACCESS_FINE_LOCATION"/>
    <uses-permission android:name="android.permission.ACCESS_COARSE_LOCATION"/>


In the activity where the permissions are required. Note it is important to check for permissions in any activity that requires permissions, as the permissions can be revoked while the app is in the background and the app will then crash.

    final private int REQUEST_CODE_ASK_MULTIPLE_PERMISSIONS = 124;
  
 
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.act_layout);
        
        // A simple check of whether runtime permissions need to be managed 
        if (Build.VERSION.SDK_INT >= 23) {
            checkMultiplePermissions();
        }
        
        
 
We only need to ask for permission for one of these from each group and all other permissions from this group are granted unless the permission is revoked by the user.
       
    private void checkMultiplePermissions() {

        if (Build.VERSION.SDK_INT >= 23) {
            List<String> permissionsNeeded = new ArrayList<String>();
            List<String> permissionsList = new ArrayList<String>();
            
            if (!addPermission(permissionsList, android.Manifest.permission.ACCESS_FINE_LOCATION)) {
                permissionsNeeded.add("GPS");
            }

            if (!addPermission(permissionsList, android.Manifest.permission.READ_EXTERNAL_STORAGE)) {
                permissionsNeeded.add("Read Storage");
            }
            
            if (permissionsList.size() > 0) {
                requestPermissions(permissionsList.toArray(new String[permissionsList.size()]),
                        REQUEST_CODE_ASK_MULTIPLE_PERMISSIONS);
                return;
            }
        }
    }
    

   
    private boolean addPermission(List<String> permissionsList, String permission) {
        if (Build.VERSION.SDK_INT >= 23)

            if (checkSelfPermission(permission) != PackageManager.PERMISSION_GRANTED) {
                permissionsList.add(permission);

                // Check for Rationale Option
                if (!shouldShowRequestPermissionRationale(permission))
                    return false;
            }
        return true;
    }
    

This deals with the result of the user allowing or not allowing permissions. In this example, if the permissions are not allowed, the app is killed.

    @Override
    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        switch (requestCode) {
            case REQUEST_CODE_ASK_MULTIPLE_PERMISSIONS: {
            
                Map<String, Integer> perms = new HashMap<String, Integer>();
                // Initial
                perms.put(android.Manifest.permission.ACCESS_FINE_LOCATION, PackageManager.PERMISSION_GRANTED);
                perms.put(android.Manifest.permission.READ_EXTERNAL_STORAGE, PackageManager.PERMISSION_GRANTED);
                
                // Fill with results
                for (int i = 0; i < permissions.length; i++)
                    perms.put(permissions[i], grantResults[i]);
                if (perms.get(android.Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED
                        && perms.get(android.Manifest.permission.READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) {
                    // All Permissions Granted
                    return;
                } else {
                    // Permission Denied
                    if (Build.VERSION.SDK_INT >= 23) {
                        Toast.makeText(
                                getApplicationContext(),
                                "My App cannot run without Location and Storage " +
                                        "Permissions.\nRelaunch My App or allow permissions" +
                                        " in Applications Settings",
                                Toast.LENGTH_LONG).show();
                        finish();
                    }
                }
            }
            break;
            default:
                super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        }
    }

More Information https://inthecheesefactory.com/blog/things-you-need-to-know-about-android-m-permission-developer-edition/en

## Enforcing Permissions in Broadcasts, URI
You can do a permissions check when sending an Intent to a registered broadcast receiver. The permissions you send are cross-checked with the ones registered under the <receiver> tag. They restrict who can send broadcasts to the associated receiver. 

To send a broadcast request with permissions, specify the permission as a string in the `Context.sendBroadcast(Intent intent, String permission)` call, but keep in mind that the receiver's app **MUST**  have that permission in order to receive your broadcast. The reciever should be installed first before the sender. 

**The method signature is:**

     void sendBroadcast (Intent intent, String receiverPermission)
     //for example to send a broadcast to Bcastreceiver receiver
     Intent broadcast = new Intent(this, Bcastreceiver.class);
     sendBroadcast(broadcast, "org.quadcore.mypermission");

and you can specify in your manifest that the broadcast sender is required to include the requested permission sent through the sendBroadcast:

     <!--  Your special permission -->
     <permission android:name="org.quadcore.mypermission" 
        android:label="my_permission" 
        android:protectionLevel="dangerous"></permission>
     
Also declare the permission in the manifest of the application that is supposed to receive this broadcast: 

     <!--  I use the permission ! -->
     <uses-permission android:name="org.quadcore.mypermission"/>
     <!-- along with the receiver -->
     <receiver android:name="Bcastreceiver" android:exported="true" />

**Note:**  Both a receiver and a broadcaster can require a permission, and when this happens, both permission checks must pass for the Intent to be delivered to the associated target. The App that defines the permission should be installed first.

Find the full documentation [here][2] on Permissions.


  [1]: http://stackoverflow.com/a/4426512/1156363
  [2]: https://developer.android.com/guide/topics/security/permissions.html

## Using PermissionUtil
[PermissionUtil][1] is a simple and convenient way of asking for permissions in context. You can easily provide what should happen in case of all requested permissions granted (`onAllGranted()`), any request was denied (`onAnyDenied()`) or in case that a rational is needed (`onRational()`).

Anywhere in your AppCompatActivity or Fragment that you want to ask for user's permisssion

    mRequestObject = PermissionUtil.with(this).request(Manifest.permission.WRITE_EXTERNAL_STORAGE).onAllGranted(
                    new Func() {
                        @Override protected void call() {
                            //Happy Path
                        }
                    }).onAnyDenied(
                    new Func() {
                        @Override protected void call() {
                            //Sad Path
                        }
                    }).ask(REQUEST_CODE_STORAGE);

And add this to `onRequestPermissionsResult`

    if(mRequestObject!=null){
        mRequestObject.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

Add the requested permission to your AndroidManifest.xml as well

    <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
        
  [1]: https://github.com/kayvannj/PermissionUtil

## Include all permission-related code to an abstract base class and extend the activity of this base class to achieve cleaner/reusable code
    public abstract class BaseActivity extends AppCompatActivity {
        private Map<Integer, PermissionCallback> permissionCallbackMap = new HashMap<>();
    
        @Override
        protected void onStart() {
            super.onStart();
            ...
        }
    
        @Override
        public void setContentView(int layoutResId) {
            super.setContentView(layoutResId);
            bindViews();
        }
    
        ...
    
        @Override
        public void onRequestPermissionsResult(
                int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
            super.onRequestPermissionsResult(requestCode, permissions, grantResults);
            PermissionCallback callback = permissionCallbackMap.get(requestCode);
    
            if (callback == null) return;
    
            // Check whether the permission request was rejected.
            if (grantResults.length < 0 && permissions.length > 0) {
                callback.onPermissionDenied(permissions);
                return;
            }
    
            List<String> grantedPermissions = new ArrayList<>();
            List<String> blockedPermissions = new ArrayList<>();
            List<String> deniedPermissions = new ArrayList<>();
            int index = 0;
    
            for (String permission : permissions) {
                List<String> permissionList = grantResults[index] == PackageManager.PERMISSION_GRANTED
                        ? grantedPermissions
                        : ! ActivityCompat.shouldShowRequestPermissionRationale(this, permission)
                            ? blockedPermissions
                            : deniedPermissions;
                permissionList.add(permission);
                index ++;
            }
    
            if (grantedPermissions.size() > 0) {
                callback.onPermissionGranted(
                        grantedPermissions.toArray(new String[grantedPermissions.size()]));
            }
    
            if (deniedPermissions.size() > 0) {
                callback.onPermissionDenied(
                        deniedPermissions.toArray(new String[deniedPermissions.size()]));
            }
    
            if (blockedPermissions.size() > 0) {
                callback.onPermissionBlocked(
                        blockedPermissions.toArray(new String[blockedPermissions.size()]));
            }
    
            permissionCallbackMap.remove(requestCode);
        }
    
        /**
         * Check whether a permission is granted or not.
         *
         * @param permission
         * @return
         */
        public boolean hasPermission(String permission) {
            return ContextCompat.checkSelfPermission(this, permission) == PackageManager.PERMISSION_GRANTED;
        }
    
        /**
         * Request permissions and get the result on callback.
         *
         * @param permissions
         * @param callback
         */
        public void requestPermission(String [] permissions, @NonNull PermissionCallback callback) {
            int requestCode = permissionCallbackMap.size() + 1;
            permissionCallbackMap.put(requestCode, callback);
            ActivityCompat.requestPermissions(this, permissions, requestCode);
        }
    
        /**
         * Request permission and get the result on callback.
         *
         * @param permission
         * @param callback
         */
        public void requestPermission(String permission, @NonNull PermissionCallback callback) {
            int requestCode = permissionCallbackMap.size() + 1;
            permissionCallbackMap.put(requestCode, callback);
            ActivityCompat.requestPermissions(this, new String[] { permission }, requestCode);
        }
    }

# Example usage in the activity

The activity should extend the abstract base class defined above as follows:
    
    private void requestLocationAfterPermissionCheck() {
        if (hasPermission(Manifest.permission.ACCESS_FINE_LOCATION)) {
            requestLocation();
            return;
        }

        // Call the base class method.
        requestPermission(Manifest.permission.ACCESS_FINE_LOCATION, new PermissionCallback() {
            @Override
            public void onPermissionGranted(String[] grantedPermissions) {
                requestLocation();
            }

            @Override
            public void onPermissionDenied(String[] deniedPermissions) {
                // Do something.
            }

            @Override
            public void onPermissionBlocked(String[] blockedPermissions) {
                // Do something.
            }
        });
    }

