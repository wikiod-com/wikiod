---
title: "Getting started with appcelerator"
slug: "getting-started-with-appcelerator"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Install Atom (and some useful packages)
Goto https://atom.io/ and install the atom editor.

Then install some Atom packages for easier Titanium coding:

|Name | Type     |  Features     |
|------------------------------    |---------------    |--------------
| [titanium language javascript](https://atom.io/packages/titanium-language-javascript) | Language | JS Autocomplete (non alloy)|                           
| [Titanium Alloy](https://atom.io/packages/titanium-alloy) | add-on| All-in-one package<br>Jump to definition<br>Open related<br>TSS Highlight|
| [Ti-Create](https://atom.io/packages/ti-create) |add-on| Create projects, controller, modules|
| [Titanium-Build](https://atom.io/packages/Titanium-Build)| add-on| Run in simulator (wip)|

Other useful non-titanium packages/add-ons:

|Name |  Features     |
|------------------------------    |--------------
| [Atom Beautify](https://atom.io/packages/atom-beautify) | Code beautifier (tss, xml, js support)|
| [minimap](https://atom.io/packages/minimap) | A preview of the full source code.|
| [minimap-highlight-selected](https://atom.io/packages/minimap-highlight-selected) | A minimap binding for the highlight-selected package|
| [highlight-selected](https://atom.io/packages/highlight-selected) | Highlights the current word selected when double clicking|
| [pigments](https://atom.io/packages/pigments) | A package to display colors in project and files.|
| [Linter](https://atom.io/packages/linter) | A Base Linter core with Cow Powers (does nothing by itself, it's an API base)|
| [Linter-jshint](https://atom.io/packages/linter-jshint) | Linter plugin for JavaScript (this checks your JS code)|
| [DocBlockr](https://atom.io/packages/docblockr) | A helper package for writing documentation|
| [Terminal-plus](https://atom.io/packages/terminal-plus) | A terminal package for Atom, complete with themes and more|
| [Project Manager](https://atom.io/packages/project-manager) | Project manager|

*source: https://github.com/m1ga/titanium_with_atom#install-atom-and-some-useful-packages*


## Installation or Setup
## Installing Appcelerator Titanium

At first we need to set up Titanium:
* command-line tools (CLI) to compile the apps
* the MVC framework [Alloy](https://docs.appcelerator.com/platform/latest/#!/guide/Alloy_Framework)
* the [SDK](https://docs.appcelerator.com/platform/latest/#!/guide/Titanium_SDK)

The main parts are installed using the node.js package manager 'npm'. Check https://nodejs.org/ if you need to install it.

### Linux (Fedora)

If you are using Fedora 23 you can run the following commands to get the needed libraries:
```bash
# install tools and libraries needed for android sdk
dnf install nodejs npm git gcc glibc.i686 glibc-devel.i686 libstdc++.i686 zlib-devel.i686 ncurses-devel.i686 libX11-devel.i686 libXrender.i686 libXrandr.i686

# intall npm version 4.2.2
npm install -g npm
npm install n -g
n 4.2.2

# install cli tools
npm install -g titanium alloy appcelerator tisdk
```
* Install Java JDK 8: http://www.if-not-true-then-false.com/2014/install-oracle-java-8-on-fedora-centos-rhel/
* Download Android SDK (SDK Tools only): https://developer.android.com/sdk/index.html#Other
* Unzip Android SDK and run android to install SDK
* adjust your .bash_profile:
```bash
echo " PATH=$PATH:$HOME/.local/bin:$HOME/android-sdk-linux/tools:$HOME/android-sdk-linux/platform-tools:/usr/java/latest/bin"
echo " export ANDROID_SDK=$HOME/android-sdk-linux"
echo " export JAVA_HOME=/usr/java/latest"
echo "export PATH"
```

### OSX / Windows

*TODO: install node/npm on Windows / OSX*

Open a console and run the following command to install the tools:

~~~ bash
npm install -g titanium alloy tisdk
~~~

## Titanium SDK

After that we need to install the SDK. To do this we will the cli tool tisdk from David Bankier (https://github.com/dbankier/tisdk):

~~~bash
# list available titanium sdks
tisdk list
~~~

The output will be something like this

~~~
4.1.0.GA
4.1.0.Beta
4.0.0.RC5
4.0.0.RC4
4.0.0.RC3
4.0.0.RC2
4.0.0.RC
4.0.0.GA
...
~~~

From this list we select the latest GA (4.1.0) and install it

~~~
tisdk install 4.1.0.GA
~~~

with this command you can check if titanium found the sdk:
~~~bash
ti sdk list
~~~

and with
~~~bash
ti info
~~~
you can see if something is missing (How to install JDK and the Android SDK will follow)

You are ready to create titanium/alloy projects now and compile them! Time to set up the editor

### get the newest SDK
The newest SDK is not available as a binary with tisdk. You have to compile it with:
~~~bash
tisdk build 5.0.0.GA
~~~
For more information, visit https://github.com/dbankier/tisdk and have a look at "Manual builds"

#### other methods
* [Codexcast](https://codexcasts.com/) released a video about "[Getting Setup With Titanium Mobile OSS: including compiling the SDK](https://codexcasts.com/episodes/getting-setup-with-titanium-mobile-oss-including-compiling-the-sdk)"
* get the unofficial nightly builds at http://builds.appcelerator.com.s3.amazonaws.com/index.html#master

*source: https://github.com/m1ga/titanium_with_atom#installing-appcelerator-titanium*

## Create your first app
We are just creating an empty Alloy app using CLI and Atom.

Open a new terminal and add the following:
~~~bash
ti create --id com.test -d . -n APPNAME -p all -t app -u http://migaweb.de
cd APPNAME/
alloy new
~~~

This will create a basic app (name: APPNAME, bundle identifier: com.test, type: app, platform: all) and then convert it into an Alloy project.

You can also use the Atom package ti-create

![main view](images/ti_create.png)

It will create a new project inside the folder that is open in the tree-view. 'Create controller/widget' only works inside an existing Alloy project ("Open folder" — select the project folder).

*source: https://github.com/m1ga/titanium_with_atom#create-your-first-app*

## Compile your app
There are several ways to compile your app. You can use the simulator/emulator, deploy it to your device or create store apk's/ipa's. There is also a live test tool (TiShadow) which saves you a lot of time waiting for the compiler.

### cli way


~~~bash
# android to device
ti build -p android  -T device

# android to store/file
ti build -p android -K /home/user/keyfile.keystore -T dist-playstore

# iOS simulator - will show a menu to select the size/device
ti build -p ios -C ?

# iOS to ipa - will show a menu to select the keys
ti build -p ios --deploy-type production --ios-version 9.0 --keychain --target dist-adhoc --output-dir .
~~~

##### iOS related

To list all distribution names you can use:
~~~bash
security find-identity -v -p codesigning
~~~

*source: https://github.com/m1ga/titanium_with_atom#compile-your-app*


