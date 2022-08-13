---
title: "Interfaces"
slug: "interfaces"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Custom Listener
Define interface
===============

    //In this interface, you can define messages, which will be send to owner.
    public interface MyCustomListener {
        //In this case we have two messages, 
        //the first that is sent when the process is successful.
        void onSuccess(List<Bitmap> bitmapList);
        //And The second message, when the process will fail.
        void onFailure(String error);
    }

Create listener
===============
In the next step we need to define an instance variable in the object that will send callback via `MyCustomListener`. And add setter for our listener.

    public class SampleClassB {
        private MyCustomListener listener;

        public void setMyCustomListener(MyCustomListener listener) {
            this.listener = listener;
        }
    }

Implement listener
==================
Now, in other class, we can create instance of `SampleClassB`.

    public class SomeActivity extends Activity {
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            SampleClassB sampleClass = new SampleClassB();
        }
    }

next we can set our listener, to `sampleClass`, in two ways:

by implements `MyCustomListener` in our class:

    public class SomeActivity extends Activity implements MyCustomListener {
        
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            SampleClassB sampleClass = new SampleClassB();
            sampleClass.setMyCustomListener(this);
        }

        @Override
        public void onSuccess(List<Bitmap> bitmapList) {

        }

        @Override
        public void onFailure(String error) {

        }
    }

or just instantiate an anonymous inner class:

    public class SomeActivity extends Activity {
        
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            SampleClassB sampleClass = new SampleClassB();
            sampleClass.setMyCustomListener(new MyCustomListener() {

                @Override
                public void onSuccess(List<Bitmap> bitmapList) {

                }

                @Override
                public void onFailure(String error) {

                }
            });
        }
    }

Trigger listener
================

    public class SampleClassB {
        private MyCustomListener listener;
    
        public void setMyCustomListener(MyCustomListener listener) {
            this.listener = listener;
        }
    
        public void doSomething() {
            fetchImages();
        }
    
        private void fetchImages() {
            AsyncImagefetch imageFetch = new AsyncImageFetch();
            imageFetch.start(new Response<Bitmap>() {
                @Override
                public void onDone(List<Bitmap> bitmapList, Exception e) {
                    //do some stuff if needed

                    //check if listener is set or not.
                    if(listener == null)
                        return;
                    //Fire proper event. bitmapList or error message will be sent to
                    //class which set listener.
                    if(e == null)
                        listener.onSuccess(bitmapList);
                    else
                        listener.onFailure(e.getMessage());
                }
            });
        }
    }



## Basic Listener


