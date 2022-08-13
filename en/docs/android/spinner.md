---
title: "Spinner"
slug: "spinner"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Adding a spinner to your activity
 
In /res/values/strings.xml:

    <string-array name="spinner_options">
        <item>Option 1</item>
        <item>Option 2</item>
        <item>Option 3</item>
    </string-array>
    
In layout XML:

    <Spinner
        android:id="@+id/spinnerName"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:entries="@array/spinner_options" />
    
In Activity:

    Spinner spinnerName = (Spinner) findViewById(R.id.spinnerName);
    spinnerName.setOnItemSelectedListener(new OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
             String chosenOption = (String) parent.getItemAtPosition(position);
        }
        @Override
        public void onNothingSelected(AdapterView<?> parent) {}
    });


## Basic Spinner Example


