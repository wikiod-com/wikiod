---
title: "Podfiles"
slug: "podfiles"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

The Podfile is a text file that describes the dependencies of one or more Xcode projects.

## Creating a podfile and installing a dependency
In the directory if the xcodeproj, create a new file called Podfile. The following is an example of a Podfile that installs the pod 'AFNetworking' version 1.0 to the app MyApp.

    target 'MyApp'
    pod 'AFNetworking', '~> 1.0'

## Creating a podfile when you have to ues Swift3.0
If you want to use Swift3.0 or Bitcode you can add this code in your podfile.

    post_install do |installer|
    installer.pods_project.targets.each do |target|
        target.build_configurations.each do |config|
            config.build_settings['ENABLE_BITCODE'] = 'YES'
            config.build_settings['SWIFT_VERSION'] = '3.0'
        end
    end


## Creating a podfile when you want to use more than one target
If you want to use a podfile in more than one target, you can do like this.
You can choose the download address when you pod this lib.

    def testpods
        pod 'YSDPush', :git => 'https://github.com/youshaoduo/YSDPush.git'
    end
    
    target 'One' do
        testpods
    end
    target 'Two' do
        testpods
    end

