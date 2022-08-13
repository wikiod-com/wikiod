---
title: "Getting started with meteor"
slug: "getting-started-with-meteor"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting Started
----

# Install Meteor

## On OS X and Linux

Install the latest official Meteor release from your terminal:


```
$ curl https://install.meteor.com/ | sh
```

## On Windows

[Download the official Meteor installer here](https://install.meteor.com/windows).



# Create your app

----------

Once you've installed Meteor, create a project:

<!-- language: lang-bash -->

```
$ meteor create myapp
```

# Run it

---------

Run it locally:

<!-- language: lang-bash -->

```
$ cd myapp
$ meteor npm install
$ meteor
```
**Note:** Meteor server running on: http://localhost:3000/

Then head to http://localhost:3000 to see your new Meteor application.

------------

  - Read more about getting started with Meteor at the[ \[Meteor Guide\]][1].
  - Explore Meteor Packages at [atmosphere][2] - a modern, fast, well engineered package manager.



  [1]: http://guide.meteor.com/#quickstart
  [2]: https://atmospherejs.com/

## Sample apps
Meteor has several sample apps built-in. You can create a project with one of them and learn from how it was built. To create a sample app, install Meteor (see [Getting Started](https://www.wikiod.com/meteor/getting-started-with-meteor#Getting Started)) and then type:

```
meteor create --example <app name>
```

For example to create a sample `todos` app, write:

```
meteor create --example todos
```

To get a list of all sample apps, type:

```
meteor create --list
```

## Understanding build progress
Sometimes builds take longer than expected. There are a few environment variables you can set to better understand what's happening during the build process.

```
METEOR_DEBUG_BUILD=1       (logs progress)
METEOR_PROFILE=<n>         (logs time spent)
METEOR_DEBUG_SPRINGBOARD=1 (?)
METEOR_DEBUG_SQL=1         (logs SQLITE calls)
METEOR_PROGRESS_DEBUG=1    (? looks like it might be useful, but seems confusing)
```

Where `<n>` is a number of ms. Any process taking longer than this will be logged.

## Linux/OSX Example

```
export METEOR_DEBUG_BUILD=1
export METEOR_PROFILE=100
meteor
```

## Windows Example

```
set METEOR_DEBUG_BUILD=1
set METEOR_PROFILE=100
meteor
```

## Updating Meteor Projects & Installed Packages
The Meteor Tool will notify you when a newer release is available.

To update Meteor projects to the latest release, execute the following command inside a Meteor project:

    meteor update

In case you want to update your Meteor project to a specific Meteor release, run the following command inside the project:

    meteor update --release <release>

If you want to update all non-core packages, run:

    meteor update --packages-only

You can also update specific packages by passing their names as a command line argument to `meteor update`, for example:

    meteor update [packageName packageName2 ...]

## Build Mobile Apps
Meteor uses [Cordova][1] to package your application into a *hybrid* Mobile App. Once packaged, the App can be distributed like native Apps (through Apple App Store, Google Play Store, etc.)

1. [**Add**][2] the target platform(s) to your Meteor project:

<!-- language: lang-bash -->
```
meteor add-platform android
meteor add-platform ios # Only available with Mac OS
```


2. [**Install**][3] the Android SDK and/or Xcode (for iOS, requires Mac OS).


3. [**Run**][4] your project (start with development mode):
<!-- language: lang-bash -->
```
meteor run android # You may need to configure a default Android emulator first
```
For iOS (only available with Mac OS):
<!-- language: lang-bash -->
```
meteor run ios # This will auto start an iOS simulator
```


4. [**Build**][5] your App package for distribution:
<!-- language: lang-bash -->
```
meteor build <output_folder> --server <url_app_should_connect_to>
```
This will create `android` and/or `ios` folder(s) alongside your server bundle.

- The `android` folder contains the `release-unsigned.apk` file that you need to sign and zip align.
- The `ios` folder contains the Xcode project that you need to sign.

See also the Meteor [Mobile Apps][6] topic.  
Reference page: [Meteor Guide > Build > Mobile][7]


  [1]: https://www.wikiod.com/cordova
  [2]: https://guide.meteor.com/mobile.html#adding-platforms
  [3]: https://guide.meteor.com/mobile.html#installing-prerequisites
  [4]: https://guide.meteor.com/mobile.html#running-your-app
  [5]: https://guide.meteor.com/mobile.html#building-and-submitting
  [6]: https://www.wikiod.com/meteor/mobile-apps
  [7]: https://guide.meteor.com/mobile.html

## Managing Packages
Meteor has it's own package repository on [atmospherejs.com][1]

You can add new packages from atmosphere by running:

    meteor add [package-author-name:package-name]

For example:

    meteor add kadira:flow-router

Similarly, you can remove the same package by:

    meteor remove kadira:flow-router

To see current packages in your project, type:

    meteor list

List of packages can also be found in the file `./meteor/packages`. To add a package add the package name in this file and to remove delete it.

To add a package locally, (e.g. unpublished packages or edited version of published packages), save the package in `packages` folder in the root.


Starting with version 1.3, Meteor **added support for npm packages**.

You can use the `npm` command inside Meteor project's directory as you would normally do without Meteor, or with the `meteor npm` command, which will use the bundled version of npm.


  [1]: http://atmospherejs.com

## Checking the Version of the Meteor Tool & Meteor Projects
# Meteor Tool

To check the installed version of the Meteor tool, just run the following command outside of any Meteor projects:

    meteor --version

To get a list of all official (recommended) Meteor releases, run:

```
meteor show METEOR
```

# Meteor Projects

If you want to check the project version of Meteor, you can also execute the following command inside a project:

    meteor --version

or just print content of the file `.meteor/release`:

    cat .meteor/release

In case you want to check the version of the packages which are currently installed in your Meteor project, print the content of the file `.meteor/versions`:

    cat .meteor/versions

# Meteor Website

To see which version of Meteor a Meteor based website is running, dump the contents of `Meteor.release` in your browsers console while visiting the website:

```
Meteor.release
```

