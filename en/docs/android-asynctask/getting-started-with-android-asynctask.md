---
title: "Getting started with android-asynctask"
slug: "getting-started-with-android-asynctask"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## AsyncTask from concept to  implementation
**Concept** <br>

AsyncTask is a class that allows running operations in the background, with the results being published on the UI thread. The main purpose is to eliminate all the boilerplate code for starting/running a thread by eliminating the handlers and all the stuff that are needed for manipulating the threads. Also, the purpose of AsyncTask is to have short-time operations on a background thread (a few seconds at most), not long-time operations.
Therefore, it is important that AsyncTask not be confused with a generic threading framework.
If one needs to do long-time operations then the concurrent package is recommended.

**General considerations** <br>

AsyncTask is defined by three generic types: Params, Progress and Results. From the moment it is executed, it goes through 4 steps (methods).
First is ***onPreExecute***, where someone can define a loading dialog, or some UI message that can notify the user the execution is about to start. Next, ***doInBackground*** which is the method that is run on asynchronously on a different thread than the Ui thread. The third method is ***onProgressUpdate*** which can also run on the UI thread that can notify the user about the status. The last method called it is ***onPostExecute*** it is mainly used for publishing the results.

Below is an example on how to use an AsyncTask, returning a string. <br>
**Example 1**

  

    public class MainActivity extends AppCompatActivity {
    
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_main);
    
            Button button = (FloatingActionButton) findViewById(R.id.btn);
            button.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    executeAsyncTaskOperation();
                }
            });
        }
    
    
          private void executeAsyncTaskOperation() {
            new CustomAsyncTask(this).execute();
        }
    
        private static class CustomAsyncTask extends AsyncTask<Void, Void, String> {
    
            private Context context;
            private ProgressDialog progressDialog;
    
            public CustomAsyncTask(Context context) {
                this.context = context;
            }
    
            @Override
            protected void onPreExecute() {
                progressDialog = ProgressDialog.show(context, "Please wait...", "Loading data from web");
            }
    
            @Override
            protected String doInBackground(Void... params) {
                String object = null;
                try {
                    Log.d(CustomAsyncTask.class.getCanonicalName(), "doInBackground");
                    Thread.sleep(500);
                    //bject = "new object";
                } catch (Exception exc) {
                    Log.e(CustomAsyncTask.class.getCanonicalName(), "exception");
                    object = null;
                }
                return object;
            }
    
            @Override
            protected void onPostExecute(String s) {
                if (progressDialog != null && progressDialog.isShowing()) {
                    progressDialog.dismiss();
                }
                if (s != null) {
                    Toast.makeText(context, "finished successfully!", Toast.LENGTH_LONG).show();
                } else {
                    Toast.makeText(context, "finished unsuccessfully!", Toast.LENGTH_LONG).show();
    
                }
            }
        }
    }


   **Example 2** <br>
  
   Here, the AsyncTask is a bit different, the execute method receive a list of data to be analyzed in the background. The return result depends on this check.  

    public class MainActivity extends AppCompatActivity {
    
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_main);
            Toolbar toolbar = (Toolbar) findViewById(R.id.toolbar);
            setSupportActionBar(toolbar);
    
            FloatingActionButton fab = (FloatingActionButton) findViewById(R.id.fab);
            fab.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Snackbar.make(view, "Replace with your own action", Snackbar.LENGTH_LONG)
                            .setAction("Action", null).show();
    
                    executeAsyncTaskOperation();
                }
            });
        }
    
    
        private void executeAsyncTaskOperation() {
            Boolean[] bools = new Boolean[10];
            for (int k = 0; k < 10; k++) {
                if (k % 2 == 0) {
                    bools[k] = true;
                } else {
                    bools[k] = false;
                }
            }
            new CustomAsyncTask(this).execute(bools);
        }
    
        private static class CustomAsyncTask extends AsyncTask<Boolean, Void, Integer> {
    
            private Context context;
            private ProgressDialog progressDialog;
    
            public CustomAsyncTask(Context context) {
                this.context = context;
            }
    
            @Override
            protected void onPreExecute() {
                progressDialog = ProgressDialog.show(context, "Please wait...", "Loading data from web");
            }
    
            @Override
            protected Integer doInBackground(Boolean... params) {
                int count = 0;
                try {
                    Thread.sleep(1000);
                    Log.d(CustomAsyncTask.class.getCanonicalName(), "doInBackground");
                    for (Boolean param : params) {
                        if (param) {
                            count++;
                        }
                    }
                } catch (Exception exc) {
                    Log.e(CustomAsyncTask.class.getCanonicalName(), "exception");
                    count = 0;
                }
                return count;
            }
    
            @Override
            protected void onPostExecute(Integer s) {
                if (progressDialog != null && progressDialog.isShowing()) {
                    progressDialog.dismiss();
                }
                if (s != null && s > 0) {
                    Toast.makeText(context, "finished loading: " + s + " tasks", Toast.LENGTH_LONG).show();
                } else {
                    Toast.makeText(context, "finished unsuccessfully!", Toast.LENGTH_LONG).show();
    
                }
            }
        }
    }




