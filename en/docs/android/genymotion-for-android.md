---
title: "Genymotion for android"
slug: "genymotion-for-android"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Genymotion is a fast third-party emulator that can be used instead of the default Android emulator. In some cases it's as good as or better than developing on actual devices!

## Installing Genymotion, the free version
Step 1 - installing `VirtualBox`
------

Download and install [VirtualBox][1] according to your operating system. , it is required to run `Genymotion`.

----------

Step 2 - downloading `Genymotion`
------

Go to the [Genymotion download page][2] and download `Genymotion` according to your operating system.

Note: you will need to create a new account OR log-in with your account.

----------

Step 3 - Installing `Genymotion` 
------

if on `Linux` then refer to this [answer][3], to install and run a `.bin` file.

----------

Step 4 - Installing `Genymotion`'s emulators
------

- run `Genymotion`
- Press on the Add button (in top bar).
- Log-In with your account and you will be able to browse the available emulators.
- select and Install what you need.

----------

Step 5 - Integrating `genymotion` with `Android Studio`
------

`Genymotion`, can be integrated with `Android Studio` via a plugin, here the steps to install it in `Android Studio`

 - go to File/Settings  (for Windows and Linux) or to Android Studio/Preferences (for Mac OS X)
 - Select Plugins and click Browse Repositories.
 - Right-click on Genymotion and click Download and install.

You should now be able to see the plugin icon, see this [image][4]

Note, you might want to display the toolbar by clicking View > Toolbar.


----------

Step 6 - Running `Genymotion` from `Android Studio`
------

 - go to File/Settings  (for Windows and Linux) or to Android Studio/Preferences (for Mac OS X)
 - go to Other Settings/Genymotion and add the path of `Genymotion's` folder and apply your changes.

----------


Now you should be able to run `Genymotion's` emulator by pressing the plugin icon and selecting an installed emulator and then press start button!

  [1]: https://www.virtualbox.org/wiki/Downloads
  [2]: https://www.genymotion.com/download/
  [3]: http://askubuntu.com/a/15094/617732
  [4]: https://www.genymotion.com/wp-content/uploads/2016/01/genymotion-android-emulator-android-studio-plugin-desktop.jpg

## Google framework on Genymotion
If developers want to test Google Maps or any other Google service like Gmail,Youtube, Google drive etc. then they first need to install Google framework on Genymotion. Here are the steps:-

[4.4 Kitkat][1]<br />
[5.0 Lollipop][2]<br />
[5.1 Lollipop][3]<br />
[6.0 Marshmallow][4]<br />
[7.0 Nougat][5]<br />
[7.1 Nougat (webview patch)][6]

<ol><li>Download from above link</li>
<li>Just drag & drop downloaded zip file to genymotion and restart</li>

<li>Add google account and download "Google Play Music" and Run.</li>
 
</ol>

**Reference:-**<br />
[Stack overflow question on this topic][7]

 


  


  [1]: http://opengapps.org/?download=true&arch=x86&api=4.4&variant=pico
  [2]: http://opengapps.org/?download=true&arch=x86&api=5.0&variant=pico
  [3]: http://opengapps.org/?download=true&arch=x86&api=5.1&variant=pico
  [4]: http://opengapps.org/?download=true&arch=x86&api=6.0&variant=pico
  [5]: http://opengapps.org/?download=true&arch=x86&api=7.0&variant=pico
  [6]: http://opengapps.org/?download=true&arch=x86&api=7.1&variant=pico
  [7]: http://stackoverflow.com/questions/17831990/how-do-you-install-google-frameworks-play-accounts-etc-on-a-genymotion-virt/40423406#40423406

