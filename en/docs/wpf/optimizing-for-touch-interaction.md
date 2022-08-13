---
title: "Optimizing for touch interaction"
slug: "optimizing-for-touch-interaction"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Showing touch keyboard on Windows 8 and Windows 10

WPF apps targeting .NET Framework 4.6.2 and later
=
With WPF apps targeting .NET Framework 4.6.2 (and later), the soft keyboard is automatically invoked and dismissed without any additional steps required.

[![WPF soft keyboard support with .NET Framework 4.6.2][1]][1]

WPF apps targeting .NET Framework 4.6.1 and earlier
=

WPF is not primarily touch enabled, which means that when the user interacts with a WPF application on desktop, the app **will not automatically display the touch keyboard** when `TextBox` controls receive focus. This is an inconvenient behavior for users of tablets, forcing them to manually open the touch keyboard via the system task bar.

[![Touch keyboard in Windows][2]][2]

Workaround
--
The touch keyboard is actually a classic `exe` application which can be found on each Windows 8 and Windows 10 PC on the following path: `C:\Program Files\Common Files\Microsoft Shared\Ink\TabTip.exe`.

Based on this knowledge, you can create a custom control derived from `TextBox`, which listens on the `GotTouchCapture` event (this event is called when the control gets focus using touch) and **starts the touch keyboard's process**.

    public class TouchEnabledTextBox : TextBox
    {
        public TouchEnabledTextBox()
        {
            this.GotTouchCapture += TouchEnabledTextBox_GotTouchCapture;
        }
    
        private void TouchEnabledTextBox_GotTouchCapture(
           object sender, 
           System.Windows.Input.TouchEventArgs e )
        {
            string touchKeyboardPath =
               @"C:\Program Files\Common Files\Microsoft Shared\Ink\TabTip.exe";        
            Process.Start( touchKeyboardPath );
        }
    }

You can improve this even further by caching the created process and then killing it after the control loses focus:

    //added field
    private Process _touchKeyboardProcess = null;
     
    //replace Process.Start line from the previous listing with
    _touchKeyboardProcess = Process.Start( touchKeyboardPath );

Now you can wire up the `LostFocus` event:

    //add this at the end of TouchEnabledTextBox's constructor
    this.LostFocus += TouchEnabledTextBox_LostFocus;
    
    //add this method as a member method of the class
    private void TouchEnabledTextBox_LostFocus( object sender, RoutedEventArgs eventArgs ){
       if ( _touchKeyboardProcess != null ) 
       {
          _touchKeyboardProcess.Kill();
          //nullify the instance pointing to the now-invalid process
          _touchKeyboardProcess = null;
       }
    }

Note about the Tablet Mode in Windows 10
--
Windows 10 introduced a **tablet mode**, which simplifies interaction with the system when using the PC in touch-first manner. This mode, apart from other improvements, ensures, that the **touch keyboard is displayed automatically** even for classic Desktop apps including WPF apps.

Windows 10 Settings approach
--
In addition to the tablet mode, Windows 10 can automatically display the touch keyboard for classic apps even outside of the tablet mode. This behavior, which is disabled by default, can be enabled in the Settings app.

In the **Settings** app, go to the **Devices** category and select **Typing**. If you scroll all the way down, you can find the "Show the touch keyboard or handwriting panel when not in tablet mode and there's no keyboard attached" setting, which you can enable.

[![Touch keyboard setting][3]][3]

It is worth mentioning, that this setting is only visible on devices with touch capabilities.


  [1]: https://i.stack.imgur.com/m8jUb.gif
  [2]: http://i.stack.imgur.com/nFKN1.jpg
  [3]: http://i.stack.imgur.com/shZDg.png

