---
title: "Cancelling an AsyncTask"
slug: "cancelling-an-asynctask"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Cancelling an AsyncTask

## Cancelling an AsyncTask
   In the following example if someone if someone presses the home button while the task is running, then the task is cancelled. In this particular cancelling it should interrupt if running. 
    
     public class MainActivity extends AppCompatActivity {
        
        private static AtomicBoolean inWork;
        private CustomAsyncTask asyncTask;
        
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_main);
            
            inWork = new AtomicBoolean(false);
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
            asyncTask = new CustomAsyncTask(this);
            asyncTask.execute(bools);
        }
        
        //pressing the home button while the task is running will trigger the onStop being called.
        @Override
        protected void onStop() {
            if (asyncTask.getStatus() == AsyncTask.Status.RUNNING) {
                asyncTask.cancel(true);
            }
            super.onStop();
        }
        
        private static class CustomAsyncTask extends AsyncTask<Boolean, Void, Integer> {
        
            private Context context;
            private ProgressDialog progressDialog;
        
            public CustomAsyncTask(Context context) {
                this.context = context;
            }
        
            @Override
            protected void onCancelled() {
                inWork.set(false);
                if (progressDialog != null && progressDialog.isShowing()) {
                    progressDialog.dismiss();
                    Log.d(CustomAsyncTask.class.getCanonicalName(), "progressdialog is dismissed.");
                }
            }
        
            @Override
            protected void onPreExecute() {
                progressDialog = ProgressDialog.show(context, "Please wait...", "Loading data from web");
            }
        
            @Override
            protected Integer doInBackground(Boolean... params) {
                int count = 0;
                inWork.set(true);
                try {
                    Thread.sleep(1000);
                    Log.d(CustomAsyncTask.class.getCanonicalName(), "doInBackground");
                    if (!isCancelled()) {
                        for (Boolean param : params) {
                            if (param) {
        
                                count++;
                            }
                        }
                    } else {
                        Log.d(CustomAsyncTask.class.getCanonicalName(), "doInBackground is cancelled.");
                    }
        
                } catch (Exception exc) {
                    Log.e(CustomAsyncTask.class.getCanonicalName(), "exception");
                    count = 0;
                }
                return count;
            }
        
            @Override
            protected void onPostExecute(Integer s) {
                if (!isCancelled()) {
                    inWork.set(false);
                    if (progressDialog != null && progressDialog.isShowing()) {
                        progressDialog.dismiss();
                    }
                    if (s != null && s > 0) {
                        Toast.makeText(context, "finished loading: " + s + " tasks", Toast.LENGTH_LONG).show();
                    } else {
                        Toast.makeText(context, "finished unsuccessfully!", Toast.LENGTH_LONG).show();
        
                    }
                } else {
                    Log.d(CustomAsyncTask.class.getCanonicalName(), "onPostExecute is cancelled.");
                }
            }
        }
    }



