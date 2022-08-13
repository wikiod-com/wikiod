---
title: "Xamarin Gesture"
slug: "xamarin-gesture"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Gesture Event
When we put the control of Label, the Label does not provide any event.
<Label x:Name="lblSignUp Text="Dont't have account?"/>
as shown the Label only display purpose only.

When the user want to replace Button with Label, then we give the event for Label.
As shown below:
 
XAML

    <Label x:Name="lblSignUp" Text="Don't have an account?" Grid.Row="8" Grid.Column="1" Grid.ColumnSpan="2">
      <Label.GestureRecognizers>
        <TapGestureRecognizer
       Tapped="lblSignUp_Tapped"/>
      </Label.GestureRecognizers>
</Label>

C#

    var lblSignUp_Tapped = new TapGestureRecognizer();   
    lblSignUp_Tapped.Tapped += (s,e) =>
    {
    //
    //  Do your work here.
    //
    };
    lblSignUp.GestureRecognizers.Add(lblSignUp_Tapped);

The Screen Below shown the Label Event.
Screen 1 : The Label "Don't have an account?" as shown in Bottom .
[![The Label "Don't have an account?" as shown in Bottom .][1]][1]

When the User click the Label "Don't have an account?", it will Navigate to Sign Up Screen.
[![enter image description here][2]][2]
For more details:
[https://developer.xamarin.com/guides/xamarin-forms/user-interface/gestures/tap/][1]


  [1]: https://i.stack.imgur.com/6Bria.png
  [2]: https://i.stack.imgur.com/IEBKb.png

