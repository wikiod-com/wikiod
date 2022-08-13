---
title: "Weak Callbacks"
slug: "weak-callbacks"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Weak callbacks are primarily used for cleaning up C++ objects embedded in the `InternalField` of a `v8::Object` created from a `v8::ObjectTemplate`.   When the JavaScript object is garbage collected, often times the C++ object must be deleted as well.  By setting a weak callback, you can get notification that a javascript object has been garbage collected and take appropriate action.  

It is *VERY* important to remember that garbage collection is *NOT* deterministic.   Your program may exit with objects with weak reference callbacks registered that are never called.  These callbacks are important for a properly behaving long-running program, but should not be relied on for releasing critical-path resources in a consistent or prompt fashion.

In order for the garbage collector to know when it should run, you have to tell it about the amount of space your C++ objects are using via the `v8::Isolate::AdjustAmountOfExternalAllocatedMemory` call.   The parameter to this call is the *change* in bytes, so when you allocate it, you'd often send in `sizeof(T)` and when you clean up in your weak reference callback, you'd send in `-sizeof(T)`.

## Running user-specified code when an Object is garbage collected.
<!-- language: c++ -->

    /**
     * Runs user-specified code when the given javascript object is garbage collected
     */
    template<class CALLBACK_FUNCTION>
    void global_set_weak(v8::Isolate * isolate, const v8::Local<v8::Object> & javascript_object, CALLBACK_FUNCTION function)
    {
        struct SetWeakCallbackData{
            SetWeakCallbackData(CALLBACK_FUNCTION function, v8::Isolate * isolate, const v8::Local<v8::Object> & javascript_object) :
                function(function) {
                    this->global.Reset(isolate, javascript_object);
            }
            // function to call for cleanup
            CALLBACK_FUNCTION function;

            // this is the weak reference
            v8::Global<v8::Object> global;
        };
    
        // This must be dynamically allocated so it sticks around until the object
        //   is garbage collected.   It cleans itself up in the callback.
        auto callback_data = new SetWeakCallbackData(function, isolate, javascript_object);
    
        // set the callback on the javascript_object to be called when it's garbage collected
        callback_data->global.template SetWeak<SetWeakCallbackData>(callback_data,
            [](const v8::WeakCallbackInfo<SetWeakCallbackData> & data) {
                SetWeakCallbackData * callback_data = data.GetParameter();
                callback_data->function(); // run user-specified code
                callback_data->global.Reset(); // free the V8 reference
                delete callback_data; // delete the heap variable so it isn't leaked
            }, v8::WeakCallbackType::kParameter);
    }



