---
title: "Display Alert"
slug: "display-alert"
draft: false
images: []
weight: 8919
type: docs
toc: true
---

## DisplayAlert
An alert box can be popped-up on a `Xamarin.Forms Page` by the method, `DisplayAlert`.  We can provide a Title, Body (Text to be alerted) and one/two Action Buttons. `Page` offers two overrides of `DisplayAlert` method.

1. `public Task DisplayAlert (String title, String message, String cancel)`

This override presents an alert dialog to the application user with a single cancel button. The alert displays modally and once dismissed the user continues interacting with the application.

Example :

    DisplayAlert ("Alert", "You have been alerted", "OK");

Above snippet will present a native implementation of Alerts in each platform (`AlertDialog` in Android, `UIAlertView` in iOS, `MessageDialog` in Windows) as below.

[![enter image description here][1]][1]

2. `public System.Threading.Tasks.Task<bool> DisplayAlert (String title, String message, String accept, String cancel)`

This override presents an alert dialog to the application user with an accept and a cancel button. It captures a user's response by presenting two buttons and returning a `boolean`. To get a response from an alert, supply text for both buttons and await the method. After the user selects one of the options the answer will be returned to the code.

Example :

    var answer = await DisplayAlert ("Question?", "Would you like to play a game", "Yes", "No");
    Debug.WriteLine ("Answer: " + (answer?"Yes":"No"));

Example 2:(if Condition true or false check to alert proceed)

    async void listSelected(object sender, SelectedItemChangedEventArgs e)
        {
            var ans = await DisplayAlert("Question?", "Would you like Delete", "Yes", "No");
            if (ans == true)
            {
                //Success condition
            }
            else
            {
                //false conditon
            }
         }


[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/9etf8.png
  [2]: http://i.stack.imgur.com/TcJYq.png

## Alert Example with only one button and action
    var alertResult = await DisplayAlert("Alert Title", Alert Message, null, "OK");
    if(!alertResult)
    {
       //do your stuff.
    }
Here we will get Ok click action.

