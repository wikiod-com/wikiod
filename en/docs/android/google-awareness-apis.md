---
title: "Google Awareness APIs"
slug: "google-awareness-apis"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Remember, the [Snapshot API][2] is used to request current state while the [Fence API][3] continuously checks for a specified state and sends callbacks when an app isn't running.

Overall, there are a few basic steps in order to use the Snapshot API or Fence API:
 - Get an API key from the [Google Developers Console][1]
 - Add necessary permissions and API key to the manifest:

        <!-- Not required for getting current headphone state -->
        <uses-permission android:name="android.permission.ACCESS_FINE_LOCATION"/>
        <!-- Only required for actvity recognition -->
        <uses-permission android:name="com.google.android.gms.permission.ACTIVITY_RECOGNITION"/>
    
        <!-- Replace with your actual API key from console -->
        <meta-data android:name="com.google.android.awareness.API_KEY"
                   android:value="YOUR_API_KEY"/>

        <!-- Required for Snapshot API only -->
        <meta-data android:name="com.google.android.geo.API_KEY"
                   android:value="YOUR_API_KEY"/> 
    
 - Initalize the `GoogleApiClient` somewhere, preferably in your activity's onCreate() method.

        GoogleApiClient client = new GoogleApiClient.Builder(context)
            .addApi(Awareness.API)
            .build();
        client.connect();
 - Call the API of your choice
 - Parse result

An easy way to check for the needed user permission is a method such as this:

    private boolean isFineLocationGranted() {
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_FINE_LOCATION)
                    != PackageManager.PERMISSION_GRANTED) {
            Log.e(getClass().getSimpleName(), "Fine location permission not granted!");
        }
    }

  [1]: https://developers.google.com/awareness/android-api/get-a-key
  [2]: https://developers.google.com/awareness/android-api/snapshot-api-overview
  [3]: https://developers.google.com/awareness/android-api/fence-api-overview

## Get current location using Snapshot API
    // Remember to intialize your client as described in the Remarks section
    Awareness.SnapshotApi.getLocation(client)
        .setResultCallback(new ResultCallback<LocationResult>() {
            @Override
            public void onResult(@NonNull LocationResult locationResult) {
                Location location = locationResult.getLocation();
                Log.i(getClass().getSimpleName(), "Coordinates: "location.getLatitude() + "," + 
                    location.getLongitude() + ", radius : " + location.getAccuracy());
            }
        });

## Get changes in user activity with Fence API
If you want to detect when your user starts or finishes an activity such as walking, running, or any other activity of the [`DetectedActivityFence`][1] class, you can create a [fence][2] for the activity that you want to detect, and get notified when your user starts/finishes this activity. By using a `BroadcastReceiver`, you will get an `Intent` with data that contains the activity:

    // Your own action filter, like the ones used in the Manifest.
    private static final String FENCE_RECEIVER_ACTION = BuildConfig.APPLICATION_ID +
        "FENCE_RECEIVER_ACTION";
    private static final String FENCE_KEY = "walkingFenceKey";
    private FenceReceiver mFenceReceiver;
    private PendingIntent mPendingIntent;

    // Make sure to initialize your client as described in the Remarks section.
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // etc.

        // The 0 is a standard Activity request code that can be changed to your needs.
        mPendingIntent = PendingIntent.getBroadcast(this, 0, 
            new Intent(FENCE_RECEIVER_ACTION), 0);
        registerReceiver(mFenceReceiver, new IntentFilter(FENCE_RECEIVER_ACTION));

        // Create the fence.
        AwarenessFence fence = DetectedActivityFence.during(DetectedActivityFence.WALKING);
        // Register the fence to receive callbacks.
        Awareness.FenceApi.updateFences(client, new FenceUpdateRequest.Builder()
            .addFence(FENCE_KEY, fence, mPendingIntent)
            .build())
            .setResultCallback(new ResultCallback<Status>() {
                @Override
                public void onResult(@NonNull Status status) {
                    if (status.isSuccess()) {
                        Log.i(FENCE_KEY, "Successfully registered.");
                    } else {
                        Log.e(FENCE_KEY, "Could not be registered: " + status);
                    }
                }
            });
        }
    }
    
    
   Now you can receive the intent with a `BroadcastReceiver` to get callbacks when the user changes the activity:

    public class FenceReceiver extends BroadcastReceiver {

        private static final String TAG = "FenceReceiver";

        @Override
        public void onReceive(Context context, Intent intent) {
            // Get the fence state
            FenceState fenceState = FenceState.extract(intent);

            switch (fenceState.getCurrentState()) {
                case FenceState.TRUE:
                    Log.i(TAG, "User is walking");
                    break;
                case FenceState.FALSE:
                    Log.i(TAG, "User is not walking");
                    break;
                case FenceState.UNKNOWN:
                    Log.i(TAG, "User is doing something unknown");
                    break;
            }
        }
    }

  [1]: https://developers.google.com/android/reference/com/google/android/gms/awareness/fence/DetectedActivityFence
  [2]: https://developers.google.com/awareness/android-api/fence-api-overview

## Get changes for location within a certain range using Fence API
If you want to detect when your user enters a specific location, you can create a fence for the specific location with a radius you want and be notified when your user enters or leaves the location.
    
    // Your own action filter, like the ones used in the Manifest
    private static final String FENCE_RECEIVER_ACTION = BuildConfig.APPLICATION_ID +
        "FENCE_RECEIVER_ACTION";
    private static final String FENCE_KEY = "locationFenceKey";
    private FenceReceiver mFenceReceiver;
    private PendingIntent mPendingIntent;
    
    // Make sure to initialize your client as described in the Remarks section
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // etc
    
        // The 0 is a standard Activity request code that can be changed for your needs
        mPendingIntent = PendingIntent.getBroadcast(this, 0, 
            new Intent(FENCE_RECEIVER_ACTION), 0);
        registerReceiver(mFenceReceiver, new IntentFilter(FENCE_RECEIVER_ACTION));
    
        // Create the fence
        AwarenessFence fence = LocationFence.entering(48.136334, 11.581660, 25);
        // Register the fence to receive callbacks.
        Awareness.FenceApi.updateFences(client, new FenceUpdateRequest.Builder()
            .addFence(FENCE_KEY, fence, mPendingIntent)
            .build())
            .setResultCallback(new ResultCallback<Status>() {
                @Override
                public void onResult(@NonNull Status status) {
                    if (status.isSuccess()) {
                        Log.i(FENCE_KEY, "Successfully registered.");
                    } else {
                        Log.e(FENCE_KEY, "Could not be registered: " + status);
                    }
                }
            });
        }
    }

Now create a BroadcastReciver to recive updates in user state:

    public class FenceReceiver extends BroadcastReceiver {

        private static final String TAG = "FenceReceiver";
    
        @Override
        public void onReceive(Context context, Intent intent) {
            // Get the fence state
            FenceState fenceState = FenceState.extract(intent);
    
            switch (fenceState.getCurrentState()) {
                case FenceState.TRUE:
                    Log.i(TAG, "User is in location");
                    break;
                case FenceState.FALSE:
                    Log.i(TAG, "User is not in location");
                    break;
                case FenceState.UNKNOWN:
                    Log.i(TAG, "User is doing something unknown");
                    break;
            }
        }
    }

## Get current user activity using Snapshot API
For one-time, non-constant requests for a user's physical activity, use the Snapshot API:
    
    // Remember to initialize your client as described in the Remarks section
    Awareness.SnapshotApi.getDetectedActivity(client)
        .setResultCallback(new ResultCallback<DetectedActivityResult>() {
            @Override
            public void onResult(@NonNull DetectedActivityResult detectedActivityResult) {
                if (!detectedActivityResult.getStatus().isSuccess()) {
                    Log.e(getClass().getSimpleName(), "Could not get the current activity.");
                    return;
                }
                ActivityRecognitionResult result = detectedActivityResult
                    .getActivityRecognitionResult();
                DetectedActivity probableActivity = result.getMostProbableActivity();
                Log.i(getClass().getSimpleName(), "Activity received : " + 
                    probableActivity.toString());
            }
        });
  

## Get headphone state with Snapshot API
    // Remember to initialize your client as described in the Remarks section
    Awareness.SnapshotApi.getHeadphoneState(client)
        .setResultCallback(new ResultCallback<HeadphoneStateResult>() {
            @Override
            public void onResult(@NonNull HeadphoneStateResult headphoneStateResult) {
                Log.i(TAG, "Headphone state connection state: " + 
                    headphoneStateResult.getHeadphoneState()
                    .getState() == HeadphoneState.PLUGGED_IN));
            }
        });

## Get nearby places using Snapshot API
    // Remember to initialize your client as described in the Remarks section 
    Awareness.SnapshotApi.getPlaces(client)
        .setResultCallback(new ResultCallback<PlacesResult>() {
            @Override
            public void onResult(@NonNull PlacesResult placesResult) {
                List<PlaceLikelihood> likelihoodList = placesResult.getPlaceLikelihoods();
                if (likelihoodList == null || likelihoodList.isEmpty()) {
                    Log.e(getClass().getSimpleName(), "No likely places");
                }
            }
        });

As for getting the data in those places, here are some options:
    
    Place place = placeLikelihood.getPlace();
    String likelihood = placeLikelihood.getLikelihood();
    Place place = likelihood.getPlace();
    String placeName = place.getName();
    String placeAddress = place.getAddress();
    String placeCoords = place.getLatLng();
    String locale = extractFromLocale(place.getLocale()));

[1]: https://developers.google.com/awareness/android-api/snapshot-api-overview

## Get current weather using Snapshot API

    // Remember to initialize your client as described in the Remarks section
    Awareness.SnapshotApi.getWeather(client)
        .setResultCallback(new ResultCallback<WeatherResult>() {
            @Override
            public void onResult(@NonNull WeatherResult weatherResult) {
                Weather weather = weatherResult.getWeather();
                if (weather == null) {
                    Log.e(getClass().getSimpleName(), "No weather received");
                } else {
                    Log.i(getClass().getSimpleName(), "Temperature is " +
                            weather.getTemperature(Weather.CELSIUS) + ", feels like " +
                            weather.getFeelsLikeTemperature(Weather.CELSIUS) + 
                            ", humidity is " + weather.getHumidity());
                }
            }
        });

