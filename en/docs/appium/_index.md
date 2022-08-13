---
title : appium Tutorial
slug : appium-tutorial
weight : 9952
draft : false
images : []
type : docs
---

[Appium][1] is an open source, cross-platform test automation tool for native, hybrid and mobile web apps, tested on simulators (iOS, FirefoxOS), emulators (Android), and real devices (iOS, Android, FirefoxOS).

**Why Appium?**

 1. You don't have to recompile your app or modify it in any way, due to use of standard automation APIs on all platforms.
 2. You don't have to recompile your app or modify it in any way, due to use of standard automation APIs on all platforms.
You can write tests with your favorite dev tools using any [WebDriver][2]-compatible language such as [Java][3], [Objective-C][4], JavaScript with Node.js (in [promise, callback][5] or [generator][6] flavors), PHP, [Python][7], [Ruby][8], [C#][9], Clojure, or Perl with the Selenium WebDriver API and language-specific client libraries.
 3. You can use any testing framework.

Investing in the WebDriver protocol means you are betting on a single, free and open protocol for testing that has become a defacto standard. Don't lock yourself into a proprietary stack.

If you use Apple's UIAutomation library without Appium you can only write tests using JavaScript and you can only run tests through the Instruments application. Similarly, with Google's UiAutomator you can only write tests in Java. Appium opens up the possibility of true cross-platform native mobile automation.

**How It Works**

Appium drives various native automation frameworks and provides an API based on Selenium's [WebDriver JSON wire protocol][2].

Appium drives Apple's UIAutomation library for versions before iOS 10, which is based on [Dan Cuellar's][10] work on iOS Auto. With the deprecation of the UIAutomation library, all iOS 10 and future version are driven by the XCUITest framework. 

Android support uses the UiAutomator framework for newer platforms and [Selendroid][11] for older Android platforms.

FirefoxOS support leverages [Marionette][12], an automation driver that is compatible with WebDriver and is used to automate Gecko-based platforms.


  [1]: https://github.com/appium/appium
  [2]: https://w3c.github.io/webdriver/webdriver-spec.html
  [3]: https://github.com/appium/java-client
  [4]: https://github.com/appium/selenium-objective-c
  [5]: https://github.com/admc/wd
  [6]: https://github.com/jlipps/yiewd
  [7]: https://github.com/appium/python-client
  [8]: https://github.com/appium/ruby_lib
  [9]: https://github.com/appium/appium-dotnet-driver
  [10]: http://github.com/penguinho
  [11]: http://github.com/DominikDary/selendroid
  [12]: https://developer.mozilla.org/en-US/docs/Marionette

