---
title: "Hello World"
slug: "hello-world"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

`onCreate()` is the most essential part of an activity, it's where most of your activity logic goes.

## Basic Activity structure
Activity is the root `UserInterface` in `Android` and have it's own life-cycle.


MainActivity.java

    public class MainActivity extends Activity {
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            Toast.makeText(this, "Activity created sucessfully!", Toast.LENGTH_LONG).show();
        }
    }


AndroidManifest.xml (should be edited)

    <manifest ... >
        <application ... >
            <activity
                android:name=".MainActivity"
                android:theme="@android:style/Theme.AppCompat">
            </activity>
        </application>
    </manifest>

