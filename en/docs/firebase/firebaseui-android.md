---
title: "FirebaseUI (Android)"
slug: "firebaseui-android"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Adding the dependencies
[FirebaseUI][1] is just an open-source library by Google that provides easy UI bindings for Firebase Auth and Firebase Database.

To begin adding FirebaseUI to your app, add these dependencies in your app's `build.gradle` file:

    android {
        // ...
    }

    dependencies {
        // Required for FirebaseUI Database
        compile 'com.google.firebase:firebase-database:9.4.0'
        compile 'com.firebaseui:firebase-ui-database:0.5.1'

        // FirebaseUI Auth only
        compile 'com.google.firebase:firebase-auth:9.4.0'
        compile 'com.firebaseui:firebase-ui-auth:0.5.1'

        // Single dependency if you're using both
        compile 'com.firebaseui:firebase-ui:0.5.1'
    }

    apply plugin: 'com.google.gms.google-services'



[1]: https://github.com/firebase/FirebaseUI-Android

## Populating a ListView
Assuming you have already set up an app in Android Studio, add a `ListView` to a layout
 (or skip if that's already done):

    <?xml version="1.0" encoding="utf-8"?>
    <android.support.design.widget.CoordinatorLayout
        xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        android:layout_width="match_parent"
        android:layout_height="match_parent">

    <!-- Your toolbar, etc -->

    <ListView
        android:id="@+id/list_view"
        android:layout_width="match_parent"
        android:layout_height="wrap_content" />

    </android.support.design.widget.CoordinatorLayout>

Now let's create a model for the data we're going to populate our `ListView` with:

    public class Person {

        private String name

        public Person() {
            // Constructor required for Firebase Database
        }

        public String getName() {
            return name;
        }

    }

Make sure your `ListView` has an id, then create a reference to it in your `Activity` and set its adapter to a new `FirebaseListAdapter`:

    public class MainActivity extends AppCompatActivity {

        // ...
    
        private ListView mListView;
    
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
        
            // Find the ListView
            mListView = (ListView) findViewById(R.id.list_view);
        
            /* 
             * Create a DatabaseReference to the data; works with standard DatabaseReference methods
             * like limitToLast() and etc.
             */
            DatabaseReference peopleReference = FirebaseDatabase.getInstance().getReference()
                .child("people");
    
            // Now set the adapter with a given layout
            mListView.setAdapter(new FirebaseListAdapter<Person>(this, Person.class,
                    android.R.layout.one_line_list_item, peopleReference) {
    
                // Populate view as needed
                @Override
                protected void populateView(View view, Person person, int position) {
                    ((TextView) view.findViewById(android.R.id.text1)).setText(person.getName());
                }
            });
        }
    }

After you've done that, add some data to your database and watch the `ListView` populate itself.


