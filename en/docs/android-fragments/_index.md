---
title : android-fragments Tutorial
slug : android-fragments-tutorial
weight : 9976
draft : false
images : []
type : docs
---

Fragments are very important components of user interface in android apps. They were introduced first in Android 3.0 (Honeycomb) API.


----------


**Understanding design paradigm of Fragments**

Fragments were introduced for primarily supporting modular and flexible UI on large screen devices such as tablets.

Fragments are managed by an activity. Usually each fragment represents a portion of a screen. There can be more than one fragment in an activity. Fragments may also be called *sub-activities*. When you add a fragment as a part of your activity layout, it lives in a ViewGroup inside the activity's view hierarchy and the fragment defines its own view layout.


----------


**LIFECYCLE**

Just like an activities, fragments also have a lifecycle. A fragment gets notified for following events.
01. Get attached to activity - **onAttach(Activity)**
02. Create fragment - **onCreate(Bundle)**
03. Create View - **onCreateView(LayoutInflater, ViewGroup, Bundle)**
04. Activity creation - **onActivityCreated(Bundle)**
05. View state restored - **onViewStateRestored(Bundle)**
05. Made visible to user - **onStart()**
06. start of user interaction - **onResume()**
07. pause of user interaction - **onPause()**
08. Made invisible to user - **onStop()**
09. On view destruction - **onDestroyView()**
10. Destroy fragment - **onDestroy()**
11. Get detached from an activity - **onDetach()**


As a programmer, you should override various lifecycle callback methods, typically we implement onCreate(), onCreateView() and onPause() methods.


----------


**Subclasses of Fragment**
1. DialogFragment - For displaying the floating dialog
2. ListFragment - For displaying list of items
3. PreferenceFragment - Useful for creating settings activity


----------
**References**

1. https://developer.android.com/guide/components/fragments.html
2. https://developer.android.com/reference/android/app/Fragment.html


