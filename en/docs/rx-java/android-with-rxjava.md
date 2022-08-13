---
title: "Android with RxJava"
slug: "android-with-rxjava"
draft: false
images: []
weight: 9847
type: docs
toc: true
---

RxAndroid used to be a library with lot of features. It has been splitted in many different libraries moving from version 0.25.0 to 1.x.

A list of libraries that implement the features available before the 1.0 is maintained [here][1].


  [1]: https://github.com/ReactiveX/RxAndroid/wiki

## RxAndroid - AndroidSchedulers
This is literally the only thing you need to start using RxJava on Android.

Include RxJava and [RxAndroid][1] in your gradle dependencies:

     // use the last version
    compile 'io.reactivex.rxjava2:rxjava:2.1.1'
    compile 'io.reactivex.rxjava2:rxandroid:2.0.1'

RxAndroid main addition to RxJava is a Scheduler for the Android Main Thread or UI Thread.

In your code:

    Observable.just("one", "two", "three", "four", "five")
            .subscribeOn(Schedulers.newThread())
            .observeOn(AndroidSchedulers.mainThread())
            .subscribe(
                data -> doStuffOnMainThread(),
                error -> handleErrorOnMainThread()
            )

Or you can create a Scheduler for a custom `Looper`:

    Looper backgroundLooper = // ...
    Observable.just("one", "two", "three", "four", "five")
            .subscribeOn(Schedulers.newThread())
            .observeOn(AndroidSchedulers.from(backgroundLooper))
            .subscribe(
                data -> doStuffOnMainThread(),
                error -> handleErrorOnMainThread()
            )


For most everything else you can refer to standard RxJava documentation.

  [1]: https://github.com/ReactiveX/RxAndroid

## RxLifecycle components
The [RxLifecycle][1] library makes it easier binding observable subscriptions to Android activities and fragment lifecycle.

Keep in mind that forgetting to unsubscribe an Observable can cause memory leaks and keeping your activity / fragment alive event after it has been destroyed by the system.

Add the library to the dependencies:

    // use the last version available
    compile 'com.trello:rxlifecycle:0.6.1'
    compile 'com.trello:rxlifecycle-components:0.6.1'

Then extends `Rx*` classes:

 - `RxActivity` / `support.RxFragmentActivity` / `support.RxAppCompatActivity`
 - `RxFragment` / `support.RxFragment`
 - `RxDialogFragment` / `support.RxDialogFragment`
 - `support.RxAppCompatDialogActivity`


You are all set, when you subscribe to an Observable you can now:

    someObservable
        .compose(bindToLifecycle())
        .subscribe();

If you execute this in the `onCreate()` method of the activity it will automatically unsubscribed in the `onDestroy()`.

Tha same happens for:

 - `onStart()` -> `onStop()`
 - `onResume()` -> `onPause()`
 - `onAttach()` -> `onDetach()` *(fragment only)*
 - `onViewCreated()` -> `onDestroyView()` *(fragment only)*

As an alternative you can specify the event when you want the unsubscription to happen:

From an activity:

    someObservable
        .compose(bindUntilEvent(ActivityEvent.DESTROY))
        .subscribe();

From a Fragment:

    someObservable
        .compose(bindUntilEvent(FragmentEvent.DESTROY_VIEW))
        .subscribe();

You can also obtain the lifecycle observable using the method `lifecycle()` to listen lifecycle events directly.

RxLifecycle can also be used directly passing to it the lifecycle observable:

    .compose(RxLifecycleAndroid.bindActivity(lifecycle))

If you need to handle `Single` or `Completable` you can do it by just adding respectively `forSingle()` or `forCompletable` after the bind method:

    someSingle
        .compose(bindToLifecycle().forSingle())
        .subscribe();

It can also be used with [Navi][2] library.


  [1]: https://github.com/trello/RxLifecycle
  [2]: https://github.com/trello/navi/

## Rxpermissions
This library allows the usage of RxJava with the new Android M permission model.

**Add the library to the dependencies:**

Rxjava

    dependencies {
        compile 'com.tbruyelle.rxpermissions:rxpermissions:0.8.0@aar'
    }
Rxjava2

    dependencies {
        compile 'com.tbruyelle.rxpermissions2:rxpermissions:0.8.1@aar'
    }

**Usage**

Example (with Retrolambda for brevity, but not required):

    // Must be done during an initialization phase like onCreate
    RxPermissions.getInstance(this)
        .request(Manifest.permission.CAMERA)
        .subscribe(granted -> {
            if (granted) { // Always true pre-M
               // I can control the camera now
            } else {
               // Oups permission denied
            }
        });

Read more: https://github.com/tbruyelle/RxPermissions.

