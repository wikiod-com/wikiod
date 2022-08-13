---
title: "Fastlane"
slug: "fastlane"
draft: false
images: []
weight: 9878
type: docs
toc: true
---

## fastlane tools
[fastlane](https://fastlane.tools) is an open source build automation tool for Android and iOS for developers. It reduce your build generation time. It is a command line tool that uses [Ruby](http://ruby-lang.org/en), so you need Ruby on your computer. Most Macs already have Ruby installed by default.

# Install fastlane

 1. Open a terminal.
 2. Run `sudo gem install fastlane --verbose`
 3. If you haven’t installed the Xcode command-line tools yet, run `xcode-select --install` to install them
 4. Now, `cd` into your project folder (type <code>cd </code> [with the space at the end] and drag your project folder into the terminal)
 5. Run `fastlane init` to get fastlane setup.
 6. Now you can able to use all the Fastlane tools:


## iOS Tools ##
* [deliver][1]: Upload screenshots, metadata, and your app to the App Store
* [snapshot][2]: Automate taking localized screenshots of your iOS app on every device
* [frameit][3]: Quickly put your screenshots into the right device frames
* [pem][4]: Automatically generate and renew your push notification profiles
* [sigh][5]: Because you would rather spend your time building stuff than fighting               provisioning
* [produce][6]: Create new iOS apps on iTunes Connect and Dev Portal using the command line
* [cert][7]: Automatically create and maintain iOS code signing certificates
* [gym][8]: Building your iOS apps has never been easier
* [match][9]: Easily sync your certificates and profiles across your team using Git
* [scan][10]: The easiest way to run tests for your iOS and Mac apps
* [spaceship][11]: Ruby library to access the Apple Dev Center and iTunes Connect

## iOS TestFlight Tools ##
* [pilot][12]: The best way to manage your TestFlight testers and builds from your terminal
* [boarding][13]: The easiest way to invite your TestFlight beta testers

## Android Tools ##
* [supply][14]: Upload your Android app and its metadata to Google Play
* [screengrab][15]: Automate taking localized screenshots of your Android app on every device
 

  [1]: https://github.com/fastlane/fastlane/tree/master/deliver
  [2]: https://github.com/fastlane/fastlane/tree/master/snapshot
  [3]: https://github.com/fastlane/fastlane/tree/master/frameit
  [4]: https://github.com/fastlane/fastlane/tree/master/pem
  [5]: https://github.com/fastlane/fastlane/tree/master/sigh
  [6]: https://github.com/fastlane/fastlane/tree/master/produce
  [7]: https://github.com/fastlane/fastlane/tree/master/cert
  [8]: https://github.com/fastlane/fastlane/tree/master/gym
  [9]: https://github.com/fastlane/fastlane/tree/master/match
  [10]: https://github.com/fastlane/fastlane/tree/master/scan
  [11]: https://github.com/fastlane/fastlane/tree/master/spaceship
  [12]: https://github.com/fastlane/fastlane/tree/master/pilot
  [13]: https://github.com/fastlane/boarding
  [14]: https://github.com/fastlane/fastlane/tree/master/supply
  [15]: https://github.com/fastlane/fastlane/tree/master/screengrab

