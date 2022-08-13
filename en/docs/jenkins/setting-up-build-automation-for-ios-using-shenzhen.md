---
title: "Setting up Build Automation for iOS using Shenzhen"
slug: "setting-up-build-automation-for-ios-using-shenzhen"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## iOS Build Automation Setup using Shenzhen
**Part I : Setup the Mac machine to use shenzhen**

Go to terminal

Install Shenzhen
> sudo gem install shenzhen

> sudo gem install nomad-cli

Download XCode command line utility
> xcode-select --install

Popup shows up with the below text

> The xcode-select command requires the command line developer tools.
> Would you like to install the tools now?”

Click - Install

Create project directory

gitclone your project

> git clone https://akshat@bitbucket.org/company/projectrepo.git

Build project using below command
> ipa build --verbose

PS: If you see any errors please select the Active Provisioning Profile and commit to the project files. and perform `ipa build --verbose` again.




