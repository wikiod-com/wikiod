---
title: "Mobile Apps"
slug: "mobile-apps"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Page Layout on Different Devices - CSS
If your application is going to run on different devices, it's going to need to render to different ViewPorts, based on the device size. You can deal with this in two ways: with javascript rules, or CSS media styles. If you've been using a MVC or MVVM library, such as Angular or Ember (or Blaze, for that matter) and have only been targeting a single device or hardware platform, you may need to rethink your MVC model as different hardware ViewPorts are introduced to your application.

```css
// desktop 
@media only screen and (min-width: 960px) {
}

// landscape orientation
@media only screen and (min-width: 768px) {
}

// portrait orientation
@media only screen and (min-width: 480px) {
}
```

You'll need to figure out if you want to break the styles at 768px (portrait mode) or at 1024 pixels (landscape). That's assuming your target mobile device is the iPad, which uses a 3:4 ratio. Otherwise, you'll need to work out the aspect ratios of the devices you do want to target, and figure out the threshold levels from there.

## Fixed Sized Windows
If you're going to be designing layouts with fixed size screens for different mobile devices, you may want to mirror that design when running your app on a desktop. The following method fixes the size of the window OUTSIDE of PhoneGap, giving a fixed-sized window on the desktop. Sometimes it's easiest to manage user's expectations and UI design by limiting options!

```js
// create a window of a specific size
var w=window.open('','', 'width=100,height=100');
w.resizeTo(500,500);

// prevent window resize
var size = [window.width,window.height];  //public variable
$(window).resize(function(){
    window.resizeTo(size[0],size[1]);
});
```

## Offline Caching
To get all of this to work, you'll probably need offline support, which means caching application data and user data.

```
meteor add appcache
meteor add grounddb
```

## Disable Scroll-Bounce
On desktop apps, you may want to disable scroll-bounce, to give your app a more native feel. You can do this with javascript, by disabling how the browser controls the DOM:

```js
// prevent scrolling on the whole page
// this is not meteorish; TODO: translate to meteor-centric code
document.ontouchmove = function(e) {e.preventDefault()};

// prevent scrolling on specific elements
// this is not meteorish; TODO: translate to meteor-centric code
scrollableDiv.ontouchmove = function(e) {e.stopPropagation()};
```

Alternatively, you can use css, and the overflow and scrolling styles.

```css
#appBody {
  overflow: hidden;
}

#contentContainer {
  .content-scrollable {
    overflow-y: auto;
    -webkit-overflow-scrolling: touch;
  }
}
```

The object model needed for the above to work looks something like this:

```html
<div id="appBody">
  <div id="contentContainer">
    <div class="content-scrollable">
      <!-- content -->
    </div>
  </div>
</div>
```

## Multitouch & Gestures
Mobile devices generally don't have keyboards, so you'll need to add some haptic controllers to your application. The two popular packages that people seem to be using is FastClick and Hammer. Installation is easy.

```
meteor add fastclick
meteor add hammer:hammer
```

FastClick requires nearly no configuration, while Hammer requires a bit of work to wire up. The cononical example from the Todos app looks like this:

```
Template.appBody.onRendered(function() {
  if (Meteor.isCordova) {
    // set up a swipe left / right handler
    this.hammer = new Hammer(this.find('#appBody'));
    this.hammer.on('swipeleft swiperight', function(event) {
      if (event.gesture.direction === 'right') {
        Session.set(MENU_KEY, true);
      } else if (event.gesture.direction === 'left') {
        Session.set(MENU_KEY, false);
      }
    });
  }
});
```


## Create your Icons and Splash Screen Assets
Before you compile your app and run it on your device, you'll need create some icons and splash screens, and add a ``mobile-config.js`` file to your app.

```js
App.icons({
  // iOS
  'iphone': 'resources/icons/icon-60x60.png',
  'iphone_2x': 'resources/icons/icon-60x60@2x.png',
  'ipad': 'resources/icons/icon-72x72.png',
  'ipad_2x': 'resources/icons/icon-72x72@2x.png',

  // Android
  'android_ldpi': 'resources/icons/icon-36x36.png',
  'android_mdpi': 'resources/icons/icon-48x48.png',
  'android_hdpi': 'resources/icons/icon-72x72.png',
  'android_xhdpi': 'resources/icons/icon-96x96.png'
});

App.launchScreens({
  // iOS
  'iphone': 'resources/splash/splash-320x480.png',
  'iphone_2x': 'resources/splash/splash-320x480@2x.png',
  'iphone5': 'resources/splash/splash-320x568@2x.png',
  'ipad_portrait': 'resources/splash/splash-768x1024.png',
  'ipad_portrait_2x': 'resources/splash/splash-768x1024@2x.png',
  'ipad_landscape': 'resources/splash/splash-1024x768.png',
  'ipad_landscape_2x': 'resources/splash/splash-1024x768@2x.png',

  // Android
  'android_ldpi_portrait': 'resources/splash/splash-200x320.png',
  'android_ldpi_landscape': 'resources/splash/splash-320x200.png',
  'android_mdpi_portrait': 'resources/splash/splash-320x480.png',
  'android_mdpi_landscape': 'resources/splash/splash-480x320.png',
  'android_hdpi_portrait': 'resources/splash/splash-480x800.png',
  'android_hdpi_landscape': 'resources/splash/splash-800x480.png',
  'android_xhdpi_portrait': 'resources/splash/splash-720x1280.png',
  'android_xhdpi_landscape': 'resources/splash/splash-1280x720.png'
});
```

## Meteor Cordova Architecture Pipeline
Now it's time to go through the [Meteor Cordova Phonegap Integration](https://guide.meteor.com/mobile.html#introduction) documentation.

Since that documentation was written, XCode and Yosemite have been released, which has caused some hiccups in installation. Here are the steps we had to go through to get Meteor compiled to an iOS device.

- Upgrade to Yosemite.
- Delete XCode (drag from Applications folder to Trashcan)
- Install XCode 6.1 from app store.
- Agree to various terms and conditions.

```js
# 5.  clone and rebuild the ios-sim locally
#     (this step will not be needed in future releases)
git clone https://github.com/phonegap/ios-sim.git
cd ios-sim
rake build

# 6.  make sure we can update the .meteor/packages locations
#     (this step will not be needed in future releases)
sudo chmod -R 777 ~/.meteor/packages

# 7.  copy the new build into Meteor locations
#     (this step will not be needed in future releases)
for i in `find ~/.meteor/packages/meteor-tool/ -name ios-sim -type f`; do
  cp -R ./build/Release/ios-sim "$i"
done

# 8.  install the ios platform to your app
cd myapp
meteor list-platforms
meteor add-platform ios
meteor list-platforms

# 9.  and that there aren't dead processes
ps -ax 
kill -9 <pid>
# /Users/abigailwatson/.meteor/packages/meteor-tool/.1.0.35.wql4jh++os.osx.x86_64+web.browser+web.cordova/meteor-tool-os.osx.x86_64/dev_bundle/mongodb/bin/mongod
# tail -f /Users/abigailwatson/Code/Medstar/dart/webapp/.meteor/local/cordova-build/platforms/ios/cordova/console.log

# 10.  make sure there are correct permissions on the application (important!)
sudo chmod -R 777 .meteor/local/

# 11.  run app
meteor run ios

# 12.  if that doesn't work, clear the directory
sudo rm -rf .meteor/local

# 13a.  run meteor again to create the default browser build
meteor

# 13b.  run it a second time so bootstrap and other packages get downloaded into the browser build
ctrl-x
meteor

# 14.  then run the ios version
ctrl-x
meteor run ios
```

XCode should launch during the process. Select your simulator and press the 'Play' button.


## IOS Development
- Register your Apple Developer Account
- Register an App ID for your app
- Register the UUID of your testing devices
- Generate an iOS App Development provisioning profile
  - Generate a CertificateSigningRequest from KeychainAccess
  - Submit CertificateSigningRequest to https://developer.apple.com/account/ios/profile/profileCreate.action
  - Download and doubleclick the certificate to import into Keychain
- Go to XCode > Preferences > Accounts and register your Apple Developer Account


## IOS Device Testing
- Make sure your development workstation and iPhone are connected to the same WiFi network. Tethering, hotspots, and other ad-hoc networking won't work.
- Run ``sudo meteor run ios-device``
- Deploy to your device!

## Configure your Cordova project (config.xml)
Meteor reads a `mobile-config.js` file in the root of your app directory during build, and uses the settings specified there to generate Cordova’s `config.xml`.
```
Project_folder
 ├── /.meteor
 └── mobile-config.js
```

Most configurations can be achieved with `mobile-config.js` (app metadata, preferences, icons and launchscreens, as well as Cordova plugins installation parameters).
<!-- language: lang-js -->
```
App.info({
  id: 'com.example.matt.uber',
  name: 'über',
  description: 'Get über power in one button click',
  author: 'Matt Development Group',
  email: 'contact@example.com',
  website: 'http://example.com'
});

// Set up resources such as icons and launch screens.
App.icons({
  'iphone': 'icons/icon-60.png',
  'iphone_2x': 'icons/icon-60@2x.png',
  // ... more screen sizes and platforms ...
});

App.launchScreens({
  'iphone': 'splash/Default~iphone.png',
  'iphone_2x': 'splash/Default@2x~iphone.png',
  // ... more screen sizes and platforms ...
});

// Set PhoneGap/Cordova preferences
App.setPreference('BackgroundColor', '0xff0000ff');
App.setPreference('HideKeyboardFormAccessoryBar', true);
App.setPreference('Orientation', 'default');
App.setPreference('Orientation', 'all', 'ios');

// Pass preferences for a particular PhoneGap/Cordova plugin
App.configurePlugin('com.phonegap.plugins.facebookconnect', {
  APP_ID: '1234567890',
  API_KEY: 'supersecretapikey'
});
```

_Do not_ manually edit the `/.meteor/local/cordova-build/config.xml` file, as it will be regenerated at every `meteor run ios/android` or `meteor build`, hence you will lose all your modifications.


Reference page: [Meteor Guide > Build > Mobile > Configuring your app][1]


  [1]: https://guide.meteor.com/mobile.html#configuring-your-app

## Detecting the deviceready event
Of course, the best way to detect mobile is for the hardware to notify you directly. Cordova PhoneGap exposes a 'deviceready' event, that you can add an event listener to. 

```
 document.addEventListener('deviceready', function(){
  Session.set('deviceready', true);
 }, false);
```

