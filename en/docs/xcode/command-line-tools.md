---
title: "Command Line Tools"
slug: "command-line-tools"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

## List available targets, schemes and build configurations
To list all available schemes for the project in your current directory

    xcodebuild -list

Optionally you can pass a path to a project or workspace file

    xcodebuild -list -workspace ./MyApp.xcworkspace
    xcodebuild -list -project ./MyApp.xcodeproj

**Example output**

    Information about project "Themoji":
        Targets:
            Themoji
            ThemojiUITests
            Unit
    
        Build Configurations:
            Debug
            Release
    
        If no build configuration is specified and -scheme is not passed then "Release" is used.
    
        Schemes:
            Themoji
            ThemojiUITests
            Units

## Running Tests
To run your unit tests in the simulator using `xcodebuild` use

If you have a workspace (e.g. when using [CocoaPods](https://cocoapods.org))

    xcodebuild \
      -workspace MyApp.xcworkspace \
      -scheme "MyScheme" \
      -sdk iphonesimulator \
      -destination 'platform=iOS Simulator,name=iPhone 6,OS=9.1' \
      test

If you have a project file

    xcodebuild \
      -project MyApp.xcproj \
      -scheme "MyScheme" \
      -sdk iphonesimulator \
      -destination 'platform=iOS Simulator,name=iPhone 6,OS=9.1' \
      test

Alternative `destination` values are

      -destination 'platform=iOS,id=REAL_DEVICE_UDID'
      -destination 'platform=iOS,name=IPHONE NAME'

## Compile and sign schema
Cleaning and compiling code for iPhone, on project MyProject for schema Qa:

    xcrun xcodebuild clean \
        -workspace "MyProject.xcworkspace" \
        -scheme "YourScheme" \
        -sdk iphoneos \
        -configuration Debug \
        archive \
        -archivePath builds/MyProject.xcarchive

Configuration can be either `Debug` or `Release`.

Signing the previously compiled code:

    xcrun xcodebuild -exportArchive \
        -archivePath builds/MyProject-Qa.xcarchive \
        -exportOptionsPlist config.plist \
        -exportPath builds

`config.plist` contains the information about how to package and sign the application, for development builds use:

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
            <key>method</key>
            <string>development</string>
            <key>uploadSymbols</key>
            <true/>
    </dict>
    </plist>
    
An App Store release plist should contain something like:

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
            <key>teamID</key>
            <string>xxxxxxxxxxx</string>
            <key>method</key>
            <string>app-store</string>
            <key>uploadSymbols</key>
            <true/>
    </dict>
    </plist>
    
Where the Team ID can be obtained from your keychain.

All available parameters
- `compileBitcode`
- `embedOnDemandResourcesAssetPacksInBundle`
- `iCloudContainerEnvironment`
- `manifest`
- `method`
- `onDemandResourcesAssetPacksBaseURL`
- `teamID`
- `thinning`
- `uploadBitcode`
- `uploadSymbols`

To get a more information about each of the parameters run `xcodebuild --help`

## Access any command line tool in Xcode app bundle (xcrun)
`xcrun` uses the system default Xcode version (set via `xcode-select`) to locate and execute command line tools from the Xcode application bundle, e.g., llvm-cov.
    
    # Generate code coverage reports via llvm-cov 
    # /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
    xcrun llvm-cov [parameters]

    # Execute xcodebuild 
    # /Applications/Xcode.app/Contents/Developer/usr/bin
    xcrun xcodebuild [parameters]

    # Use Xcode's version of git, e.g., if you have installed a newer version
    # /Applications/Xcode.app/Contents/Developer/usr/bin 
    xcrun git [parameters]

## Switching command line tools with xcode-select
Print the path to the active developer directory (selected Xcode) 

`xcode-select -p`

Select a different version of Xcode, e.g. Beta

`sudo xcode-select -s /Applications/Xcode-beta.app`

Reset to the default version of Xcode

`sudo xcode-select -r`

This is equivalent to running `sudo xcode-select -s /Applications/Xcode.app`

For more details: `man xcode-select`

