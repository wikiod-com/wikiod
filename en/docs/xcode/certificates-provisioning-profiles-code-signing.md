---
title: "Certificates, Provisioning Profiles, & Code Signing"
slug: "certificates-provisioning-profiles--code-signing"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Choose the right code signing approach
If you are just starting a new project, it's important to think about how you want to handle code signing.

If you are new to code signing, check out the [WWDC session](https://developer.apple.com/videos/play/wwdc2016/401/) that describes the fundamentals of code signing in Xcode.

To properly code-sign your app, you have to have the following resources on your local machine:

- The private key (`.p12` file)
- The certificate (`.cer` file), matching the private key
- The provisioning profile (`.mobileprovision` file), matching the certificate and private key installed locally

On the Apple Developer Portal it's also required to have a valid App ID associated with your provisioning profile.

# Using Xcode's code signing feature

Occasionally the `Automatic` setting as the provisioning profile doesn't work reliably as it will just select the most recently updated provisioning profile, no matter if the certificate is installed. 

That's why it is recommended to specify a specific provisioning profile somehow:

### Xcode 7 and lower

You should avoid clicking the `Fix Issue` button (There is an [Xcode plugin](https://github.com/neonichu/FixCode#readme) that disables the button), as it sometimes revokes existing certificates, and with it the provisioning profiles.

Unfortunately you can't specify the name of the provisioning profile in Xcode 7. Instead you can specify the UUID of the profile, which changes every time the profile gets re-generated (e.g. when you add a new device).

To work around this issue, check out [XcodeProject.md](https://github.com/fastlane/fastlane/blob/master/fastlane/docs/Codesigning/XcodeProject.md) on how to pass a provisioning profile to Xcode when building your app.

### Xcode 8 and up

Apple improved code signing a lot with the release of Xcode 8, the following has changed:

- No more `Fix Issue` button, instead all code signing processes run in the background and show the log right in Xcode
- You can now specify the provisioning profile by name, instead of the UUID (Check out [XcodeProject.md](https://github.com/fastlane/fastlane/blob/master/fastlane/docs/Codesigning/XcodeProject.md) for more information)
- Improved error messages when something goes wrong. If you run into code signing errors you should always try building and signing with Xcode to get more detailed error information. (Check out [Troubleshooting.md](https://github.com/fastlane/fastlane/blob/master/fastlane/docs/Codesigning/Troubleshooting.md) for more information)

# Manually

You can always manually create and manage your certificates and provisioning profiles using the Apple Developer Portal. Make sure to store the private key (`.p12`) of your certificates in a safe place, as they can't be restored if you lose them. 

You can always download the certificate (`.cer`) and provisioning profile (`.mobileprovision`) from the Apple Developer Portal.

If you revoke your certificate or it expires, all associated provisioning profiles will be invalid.

# Using [fastlane match](https://fastlane.tools/match)

The concept of [match](https://fastlane.tools/match) is described in the [codesigning guide](https://codesigning.guide) and is the recommended code signing approach if you use [fastlane](https://fastlane.tools)

With [match](https://fastlane.tools/match) you store your private keys and certificates in a git repo to sync them across machines. This makes it easy to onboard new team-members and set up new Mac machines. This approach [is secure](https://github.com/fastlane/fastlane/tree/master/match#is-this-secure) and uses technology you already use.

Getting started with [match](https://fastlane.tools/match) requires you to revoke your existing certificates.

