---
title: "Building Backwards Compatible Apps"
slug: "building-backwards-compatible-apps"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## How to handle deprecated API
It is unlikely for a developer to not come across a deprecated API during a development process. A deprecated program element is one that programmers are discouraged from using, typically because it is dangerous, or because a better alternative exists. Compilers and analyzers (like [LINT][1]) warn when a deprecated program element is used or overridden in non-deprecated code.

A deprecated API is usually identified in Android Studio using a strikeout. In the example below, the method `.getColor(int id)` is deprecated:

<pre><code>getResources().<s>getColor</s>(R.color.colorAccent));</code></pre>

If possible, developers are encouraged to use alternative APIs and elements. It is possible to check backwards compatibility of a library by visiting the Android documentation for the library and checking the "Added in API level x" section:

 [![getColor was added in API level 1 and deprecated in API level 23][2]][2]


In the case that the API you need to use is not compatible with the Android version that your users are using, you should check for the API level of the user before using that library. For example:

    //Checks the API level of the running device
    if (Build.VERSION.SDK_INT < 23) {
        //use for backwards compatibility with API levels below 23
        int color = getResources().getColor(R.color.colorPrimary);
    } else {
        int color = getResources().getColor(R.color.colorPrimary, getActivity().getTheme());
    }

Using this method ensures that your app will remain compatible with new Android versions as well as existing versions.

# Easier alternative: Use the Support Library

If the Support Libraries are used, often there are static helper methods to accomplish the same task with less client code. Instead of the if/else block above, just use:

    final int color = android.support.v4.content.ContextCompat
        .getColor(context, R.color.colorPrimary);

Most deprecated methods that have newer methods with a different signature and many new features that may not have been able to be used on older versions have compatibility helper methods like this. To find others, browse through the support library for classes like `ContextCompat`, `ViewCompat`, etc.


  [1]: https://www.wikiod.com/android/lint-warnings
  [2]: http://i.stack.imgur.com/qAbfb.png

