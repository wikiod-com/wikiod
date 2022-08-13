---
title: "Loader"
slug: "loader"
draft: false
images: []
weight: 9897
type: docs
toc: true
---

Loader is good choice for prevent memory leak if you want to load data in background when oncreate method is called. 
For example when we execute Asynctask in oncreate method and we rotate the screen  so the activity will recreate which will execute another AsyncTask again, so probably two Asyntask running in parallel together rather than like loader which will continue the background process we executed before.

## Parameters
| Class | Description |
| ------ | ------ |
| [LoaderManager][1]   | An abstract class associated with an [Activity][2] or [Fragment][3] for managing one or more Loader instances.   |
| [LoaderManager.LoaderCallbacks][4] | A callback interface for a client to interact with the LoaderManager.
| [Loader][5] | An abstract class that performs asynchronous loading of data. |
| [AsyncTaskLoader][6] | Abstract loader that provides an [AsyncTask][7] to do the work. |
| [CursorLoader][8] |      A subclass of AsyncTaskLoader that queries the ContentResolver and returns a Cursor. |


  [1]: https://developer.android.com/reference/android/app/LoaderManager.html
  [2]: https://developer.android.com/reference/android/app/Activity.html
  [3]: https://developer.android.com/reference/android/app/Fragment.html
  [4]: https://developer.android.com/reference/android/app/LoaderManager.LoaderCallbacks.html
  [5]: https://developer.android.com/reference/android/content/Loader.html
  [6]: https://developer.android.com/reference/android/content/AsyncTaskLoader.html
  [7]: https://developer.android.com/reference/android/os/AsyncTask.html
  [8]: https://developer.android.com/reference/android/content/CursorLoader.html

Introduced in Android 3.0, loaders make it easy to asynchronously load data in an activity or fragment. Loaders have these characteristics:

 - They are available to every [Activity][1] and [Fragment][2].
 - They provide asynchronous loading of data.
 - They monitor the source of their data and deliver new results when the content changes.
 - They automatically reconnect to the last loader's cursor when being recreated after a configuration change. Thus, they don't need to re-query their data.

<h1>When not to use Loaders</h1>
You shouldn’t use Loaders if you need the background tasks to complete. Android destroys Loaders together with the Activities/Fragments they belong to. If you want to do some tasks, that have to run until completion, do not use Loaders. You should use services for this kind of stuff instead.

  [1]: https://developer.android.com/reference/android/app/Activity.html
  [2]: https://developer.android.com/reference/android/app/Fragment.html

## Basic AsyncTaskLoader
`AsyncTaskLoader` is an abstract `Loader` that provides an `AsyncTask` to do the work.

Here some basic implementation:

    final class BasicLoader extends AsyncTaskLoader<String> {
    
        public BasicLoader(Context context) {
            super(context);
        }
    
        @Override
        public String loadInBackground() {
            // Some work, e.g. load something from internet
            return "OK";
        }
    
        @Override
        public void deliverResult(String data) {
            if (isStarted()) {
                // Deliver result if loader is currently started
                super.deliverResult(data);
            }
        }
    
        @Override
        protected void onStartLoading() {
            // Start loading
            forceLoad();
        }
    
        @Override
        protected void onStopLoading() {
            cancelLoad();
        }
    
        @Override
        protected void onReset() {
            super.onReset();
    
            // Ensure the loader is stopped
            onStopLoading();
        }
    }

Typically `Loader` is initialized within the activity's `onCreate()` method, or within the fragment's `onActivityCreated()`. Also usually activity or fragment implements `LoaderManager.LoaderCallbacks` interface:

    public class MainActivity extends Activity implements LoaderManager.LoaderCallbacks<String> {
    
        // Unique id for loader
        private static final int LDR_BASIC_ID = 1;
    
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_main);
    
            // Initialize loader; Some data can be passed as second param instead of Bundle.Empty
            getLoaderManager().initLoader(LDR_BASIC_ID, Bundle.EMPTY, this);
        }
    
        @Override
        public Loader<String> onCreateLoader(int id, Bundle args) {
            return new BasicLoader(this);
        }
    
        @Override
        public void onLoadFinished(Loader<String> loader, String data) {
            Toast.makeText(this, data, Toast.LENGTH_LONG).show();
        }
    
        @Override
        public void onLoaderReset(Loader<String> loader) {
        }
    }

In this example, when loader completed, toast with result will be shown.



## AsyncTaskLoader with cache
It's a good practice to cache loaded result to avoid multiple loading of same data.<br/> To invalidate cache `onContentChanged()` should be called. If loader has been already started, `forceLoad()` will be called, otherwise (if loader in stopped state) loader will be able to understand content change with `takeContentChanged()` check.

*Remark: `onContentChanged()` must be called from the process's main thread.*

*Javadocs says about takeContentChanged():*

> Take the current flag indicating whether the loader's content had
> changed while it was stopped.  If it had, true is returned and the
> flag is cleared.

    public abstract class BaseLoader<T> extends AsyncTaskLoader<T> {
    
        // Cached result saved here
        private final AtomicReference<T> cache = new AtomicReference<>();
    
        public BaseLoader(@NonNull final Context context) {
            super(context);
        }
    
        @Override
        public final void deliverResult(final T data) {
            if (!isReset()) {
                // Save loaded result
                cache.set(data);
                if (isStarted()) {
                    super.deliverResult(data);
                }
            }
        }
    
        @Override
        protected final void onStartLoading() {            
            // Register observers
            registerObserver();

            final T cached = cache.get();    
            // Start new loading if content changed in background
            // or if we never loaded any data
            if (takeContentChanged() || cached == null) {
                forceLoad();
            } else {
                deliverResult(cached);
            }
        }
    
        @Override
        public final void onStopLoading() {
            cancelLoad();
        }
    
        @Override
        protected final void onReset() {
            super.onReset();
            onStopLoading();
            // Clear cache and remove observers
            cache.set(null);
            unregisterObserver();
        }
    
        /* virtual */
        protected void registerObserver() {
            // Register observers here, call onContentChanged() to invalidate cache
        }
    
        /* virtual */
        protected void unregisterObserver() {
            // Remove observers
        }
    }



## Reloading
To invalidate your old data and restart existing loader you can use [`restartLoader()`][1] method:

    private void reload() {
        getLoaderManager().reastartLoader(LOADER_ID, Bundle.EMPTY, this);
    }


  [1]: https://developer.android.com/reference/android/app/LoaderManager.html#restartLoader(int,%20android.os.Bundle,%20android.app.LoaderManager.LoaderCallbacks%3CD%3E)

## Pass parameters using a Bundle
You can pass parameters by Bundle:

    Bundle myBundle = new Bundle();
    myBundle.putString(MY_KEY, myValue);

Get the value in onCreateLoader:

    @Override
    public Loader<String> onCreateLoader(int id, final Bundle args) {
        final String myParam = args.getString(MY_KEY);
        ...
    }


