---
title: "Getting started with android-volley"
slug: "getting-started-with-android-volley"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation And Setup
**Installation**

 Volley JCenter Gradle Import

<!-- language: java -->
    //in your project's app level build.gradle
    compile 'com.android.volley:volley:1.0.0'

**Create a subclass of Application**

<!-- language: java -->
    public class AppController extends Application {

        public static final String TAG = AppController.class
                .getSimpleName();
    
        private RequestQueue mRequestQueue;
        private ImageLoader mImageLoader;
    
        private static AppController mInstance;
    
        @Override
        public void onCreate() {
            super.onCreate();
            mInstance = this;
        }
    
        public static synchronized AppController getInstance() {
            return mInstance;
        }
    
        public RequestQueue getRequestQueue() {
            if (mRequestQueue == null) {
                mRequestQueue = Volley.newRequestQueue(getApplicationContext());
            }
    
            return mRequestQueue;
        }
    
        public ImageLoader getImageLoader() {
            getRequestQueue();
            if (mImageLoader == null) {
                mImageLoader = new ImageLoader(this.mRequestQueue,
                        new LruBitmapCache());
            }
            return this.mImageLoader;
        }
    
        public <T> void addToRequestQueue(Request<T> req, String tag) {
            // set the default tag if tag is empty
            req.setTag(TextUtils.isEmpty(tag) ? TAG : tag);
            getRequestQueue().add(req);
        }
    
        public <T> void addToRequestQueue(Request<T> req) {
            req.setTag(TAG);
            getRequestQueue().add(req);
        }
    
        public void cancelPendingRequests(Object tag) {
            if (mRequestQueue != null) {
                mRequestQueue.cancelAll(tag);
            }
        }
    }

**Create a StringRequest**

<!-- language: java -->
     public class StringRequestActivity extends Activity {

     private String TAG = StringRequestActivity.class.getSimpleName();
     private Button btnStringReq;
     private TextView msgResponse;
    

     // This tag will be used to cancel the request
     private String tag_string_req = "string_req";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_string);

        btnStringReq = (Button) findViewById(R.id.btnStringReq);
        msgResponse = (TextView) findViewById(R.id.msgResponse);

        btnStringReq.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                makeStringReq();
            }
        });
    }

    
    /**
     * Making json object request
     * */
    private void makeStringReq() {
        

        StringRequest strReq = new StringRequest(Method.GET,
                "http://www.myurl.com", new Response.Listener<String>() {

                    @Override
                    public void onResponse(String response) {
                        Log.d(TAG, response.toString());
                        msgResponse.setText(response.toString());
                        

                    }
                }, new Response.ErrorListener() {

                    @Override
                    public void onErrorResponse(VolleyError error) {
                        VolleyLog.d(TAG, "Error: " + error.getMessage());
                        
                    }
                });

        // Adding request to request queue
        AppController.getInstance().addToRequestQueue(strReq, tag_string_req);

      }
    }


 


**Singleton RequestQueue Setup**

Generally it's recommended that you use a single RequestQueue throughout your Application.  So, you want to have one NetworkRequestManager singleton that contains your Volley RequestQueue.  A simple implementation would be:

<!-- language: java -->
    public class NetworkRequestManager {

    private static final String TAG = NetworkRequestManager.class.getName();

    private static NetworkRequestManager mManager;

    private RequestQueue requestQueue;

    private NetworkRequestManager(@NonNull final Context context) {
       initQueue(context);
    }

    /**
     * @return A NetworkRequestManager with an initialized RequestQueue
     */
    public static synchronized NetworkRequestManager getInstance(@NonNull final Context context) {
        if(mManager == null){
            mManager = new NetworkRequestManager(context);
        }
        return mManager;
    }

    /**
     * Initialize your request queue.  This uses the default Volley
     * setup.
     *
     * @param context
     */
    private void initQueue(Context context) {
        if (requestQueue == null) {
            requestQueue = Volley.newRequestQueue(context.getApplicationContext());
        }
    }

    /**
     * @Return our initialized RequestQueue
    */
    public RequestQueue getRequestQueue() {
        return requestQueue;
    }

    /**
     * Cancels all requests for the given object tag
     *
     * @param tag
     */
    @Override
    public void cancelAllForTag(Object tag) {
        getRequestQueue().cancelAll(tag);
    }

    /**
     *
     * A convenience method for adding requests to the queue that are associated with a tag for cancellation
     *
     * @param request
     * @param tag
     */
    @Override
    public void addRequest(Request<?> request, Object tag) {
        request.setTag(tag);
        getRequestQueue().add(request);
    }







