---
title: "Getting started with android-intent"
slug: "getting-started-with-android-intent"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Start another activity - Hello World of intents
    public class CurrentActivity extends AppCompatActivity {
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.current_activity);
            
            Intent intent = new Intent(this, DestinationActivity.class);
            startActivity(intent);
        }
    }

## Installation or Setup
Detailed instructions on getting android-intent set up or installed.

