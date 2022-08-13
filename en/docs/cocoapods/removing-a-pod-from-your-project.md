---
title: "Removing a Pod from your Project"
slug: "removing-a-pod-from-your-project"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Removing a Pod
Example Podfile:

    target 'MyProject' do
        pod 'AWSS3'
        pod 'RealmSwift'
    end

and if you wanted to remove `RealmSwift`, remove that line from your Podfile

    target 'MyProject' do
        pod 'AWSS3'
    end

then from the command line run

    $ pod install

and CocoaPods will remove `RealmSwift`.

