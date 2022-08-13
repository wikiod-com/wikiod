---
title: "Toasts"
slug: "toasts"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Basic Toast Message
First, instantiate a Toast object with one of the [`MakeText()`][1] methods. This method takes three parameters: the application [`Context`][2], the text message, and the duration for the toast. It returns a properly initialized Toast object. You can display the toast notification with [`Show()`][3], as shown in the following example:

<!-- language: c# -->

    Context context = Application.Context;
    string text = "Hello toast!";
    ToastLength duration = ToastLength.Short;
    
    var toast = Toast.MakeText(context, text, duration);
    toast.Show();

This example demonstrates everything you need for most toast notifications. You should rarely need anything else. You may, however, want to position the toast differently or even use your own layout instead of a simple text message. The following sections describe how you can do these things.

You can also chain your methods, call as a one-liner and avoid holding on to the Toast object, like this:

<!-- language: c# -->

    Toast.MakeText(Application.Context, "Hello toast!", ToastLength.Short).Show();

For more information refer to the more complete [Android documentation][4] on the topic.


  [1]: https://developer.xamarin.com/api/member/Android.Widget.Toast.MakeText/
  [2]: https://developer.xamarin.com/api/property/Android.Content.ContextWrapper.ApplicationContext/
  [3]: https://developer.xamarin.com/api/member/Android.Widget.Toast.Show()/
  [4]: https://developer.android.com/guide/topics/ui/notifiers/toasts.html

## Colored Toast Messages
Sometimes we want to give extra information to our user with colors (for example red means something wrong has happened)
We can change toast message background color using setting a color filter to the view which our toast give us (here I use a [ColorMatrixColorFilter][1]):

    Toast t = Toast.MakeText(context, message, duration);
    Color c = */your color/*;
    ColorMatrixColorFilter CM = new ColorMatrixColorFilter(new float[]
        {
            0,0,0,0,c.R,
            0,0,0,0,c.G,
            0,0,0,0,c.B,
            0,0,0,1,0            
        });
    t.View.Background.SetColorFilter(CM);
    t.Show();
And also we can change the text color if background is light or dark:

    if ((((float)(c.R) + (float)(c.G) + (float)(c.B)) / 3) >= 128)
        t.View.FindViewById<TextView>(Android.Resource.Id.Message).SetTextColor(Color.Black);
    else
    //text color is white by default


  [1]: https://developer.android.com/reference/android/graphics/ColorMatrixColorFilter.html

## Change Toast Position
We can change our toast using SetGravity method.
This method takes three parameters: first is gravity of toast on screen and two others set toast offset from the starting position (which is set by the first parameter):

    //Toast at bottom left corner of screen
    Toast t = Toast.MakeText(context, message, duration);
    t.SetGravity(GravityFlags.Bottom | GravityFlags.Left, 0, 0);
    t.Show();

    //Toast at a custom position on screen
    Toast t = Toast.MakeText(context, message, duration);
    t.SetGravity(GravityFlags.Top | GravityFlags.Left, x, y);
    t.Show();

