---
title: "adb shell"
slug: "adb-shell"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

`adb shell` opens a Linux shell in a target device or emulator.
It is the most powerful and versatile way to control an Android device via `adb`.

This topic was split from https://www.wikiod.com/android/adb-android-debug-bridge due to reaching the limit of examples, many of which were involving `adb shell` command.

## Syntax
 - adb shell [-e escape] [-n] [-Tt] [-x] [command]

## Parameters
| Parameter | Details |
| --- | ---- |
|-e| choose escape character, or "none"; default '~'|
|-n| don't read from stdin|
|-T| disable PTY allocation|
|-t| force PTY allocation|
|-x| disable remote exit codes and stdout/stderr separation|
 

## Granting & revoking API 23+ permissions
A one-liner that helps granting or revoking vulnerable permissions.

 - **granting**

       adb shell pm grant <sample.package.id> android.permission.<PERMISSION_NAME>

 - **revoking**

       adb shell pm revoke <sample.package.id> android.permission.<PERMISSION_NAME>

 - **Granting all run-time permissions at a time on installation (-g)**

       adb install -g /path/to/sample_package.apk



## Send text, key pressed and touch events to Android Device via ADB
execute the following command to insert the text into a view with a focus (if it supports text input)

<!-- if version [gte 6.0] -->
**Send text on SDK 23+**

    adb shell "input keyboard text 'Paste text on Android Device'"

If already connected to your device via `adb`:

    input text 'Paste text on Android Device'
<!-- end version if -->

<!-- if version [lt 6.0] -->
**Send text prior to SDK 23**

    adb shell "input keyboard text 'Paste%stext%son%sAndroid%sDevice'"

Spaces are not accepted as the input, replace them with %s.
<!-- end version if -->

**Send events**

To simulate pressing the hardware power key

    adb shell input keyevent 26

or alternatively

    adb shell input keyevent POWER

Even if you don't have a hardware key you still can use a `keyevent` to perform the equivalent action

    adb shell input keyevent CAMERA

**Send touch event as input**

    adb shell input tap Xpoint Ypoint

**Send swipe event as input**

    adb shell input swipe Xpoint1 Ypoint1 Xpoint2 Ypoint2 [DURATION*]

*DURATION is optional, default=300ms. [source][1]

Get X and Y points by enabling pointer location in developer option.

**ADB sample shell script**



> To run a script in Ubuntu, Create script.sh right click the file and add read/write permission and tick **allow executing file as program**.

> Open terminal emulator and run the command ./script.sh

 
Script.sh
   

     for (( c=1; c<=5; c++ ))
        do  
           adb shell input tap X Y
           echo "Clicked $c times"
           sleep 5s
        done
    

     
For a comprehensive list of event numbers 
* shortlist of several interesting events http://stackoverflow.com/questions/7789826/adb-shell-input-events
* reference documentation https://developer.android.com/reference/android/view/KeyEvent.html#KEYCODE_POWER. 


  [1]: http://androidxref.com/7.1.1_r6/xref/frameworks/base/cmds/input/src/com/android/commands/input/Input.java#201

## List packages
Prints all packages, optionally only those whose package name contains the text in \<FILTER\>.

    adb shell pm list packages [options] <FILTER>

    All <FILTER>

    adb shell pm list packages

Attributes:

`-f` to see their associated file.

`-i` See the installer for the packages.

`-u` to also include uninstalled packages.

`-u` Also include uninstalled packages.

Attributes that filter:

`-d` for disabled packages.

`-e` for enabled packages.

`-s` for system packages.

`-3` for third party packages.

`--user <USER_ID>` for a specific user space to query.

## Recording the display
<!-- if version [gte 4.4] -->
Recording the display of devices running Android 4.4 (API level 19) and higher:

    adb shell screenrecord [options] <filename>
    adb shell screenrecord /sdcard/demo.mp4

(press Ctrl-C to stop recording)

Download the file from the device: 

    adb pull /sdcard/demo.mp4

> Note: Stop the screen recording by pressing Ctrl-C, otherwise the recording stops automatically at three minutes or the time limit set by `--time-limit`.

    adb shell screenrecord --size <WIDTHxHEIGHT>

Sets the video size: 1280x720. The default value is the device's native display resolution (if supported), 1280x720 if not. For best results, use a size supported by your device's Advanced Video Coding (AVC) encoder.

---

    adb shell screenrecord --bit-rate <RATE>

Sets the video bit rate for the video, in megabits per second. The default value is 4Mbps. You can increase the bit rate to improve video quality, but doing so results in larger movie files. The following example sets the recording bit rate to 5Mbps: 
    
    adb shell screenrecord --bit-rate 5000000 /sdcard/demo.mp4

---

    adb shell screenrecord --time-limit <TIME>

Sets the maximum recording time, in seconds. The default and maximum value is 180 (3 minutes).

---

    adb shell screenrecord --rotate

Rotates the output 90 degrees. This feature is experimental.

---

    adb shell screenrecord --verbose

Displays log information on the command-line screen. If you do not set this option, the utility does not display any information while running.

> Note: This might not work on some devices.
<!-- end version if -->
<!-- if version [lt 4.4] -->
The screen recording command isn't compatible with android versions pre 4.4
>The screenrecord command is a shell utility for recording the display of devices running Android 4.4 (API level 19) and higher. The utility records screen activity to an MPEG-4 file.
<!-- end version if -->

## Set Date/Time via adb
<!-- if version [gte 6.0] -->

Default SET format is `MMDDhhmm[[CC]YY][.ss]`, that's (2 digits each)

For example, to set July 17'th 10:10am, without changing the current year, type:

    adb shell 'date 07171010.00'

**Tip 1:** the date change will not be reflected immediately, and a noticable change will happen only after the system clock advances to the next minute.<br>
You can force an update by attaching a `TIME_SET` intent broadcast to your call, like that:

    adb shell 'date 07171010.00 ; am broadcast -a android.intent.action.TIME_SET'

**Tip 2:** to synchronize Android's clock with your local machine:

Linux:

    adb shell date `date +%m%d%H%M%G.%S`

Windows (PowerShell):

    $currentDate = Get-Date -Format "MMddHHmmyyyy.ss" # Android's preferred format
    adb shell "date $currentDate"

**Both tips together:**

    adb shell 'date `date +%m%d%H%M%G.%S` ; am broadcast -a android.intent.action.TIME_SET'
<!-- end version if -->

<!-- if version [lt 6.0] -->
Default SET format is 'YYYYMMDD.HHmmss'

    adb shell 'date -s 20160117.095930'

**Tip:** to synchronize Android's clock with your local (linux based) machine:

    adb shell date -s `date +%G%m%d.%H%M%S`

<!-- end version if -->

## Open Developer Options
    adb shell am start -n com.android.settings/.DevelopmentSettings

Will navigate your device/emulator to the `Developer Options` section.

## Print application data
This command print all relevant application data:     

 - version code
 - version name
 - granted permissions (Android API 23+)
 - etc..


    adb shell dumpsys package <your.package.id>

## Changing file permissions using chmod command


## Generating a "Boot Complete" broadcast
This is relevant for apps that implement a `BootListener`. Test your app by killing your app and then test with:

    adb shell am broadcast -a android.intent.action.BOOT_COMPLETED -c android.intent.category.HOME -n your.app/your.app.BootListener

(replace `your.package/your.app.BootListener` with proper values).

## View external/secondary storage content
View content:

    adb shell ls \$EXTERNAL_STORAGE
    adb shell ls \$SECONDARY_STORAGE

View path:

    adb shell echo \$EXTERNAL_STORAGE
    adb shell echo \$SECONDARY_STORAGE



## kill a process inside an Android device
Sometimes Android's logcat is running infinitely with errors coming from some process not own by you, draining battery or just making it hard to debug your code.

A convenient way to fix the problem without restarting the device is to locate and kill the process causing the problem.

From Logcat 

`03-10 11:41:40.010 1550-1627/? E/SomeProcess: ....`

notice the process number: 1550

Now we can open a shell and kill the process.
Note that we cannot kill `root` process.

    adb shell

inside the shell we can check more about the process using

    ps -x | grep 1550

and kill it if we want:

    kill -9 1550


