---
title: "Publishing A Release Track"
slug: "publishing-a-release-track"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Publishing a Release Track is actually pretty simple if you understand a) that the publish-release command requires a .json file as a parameter, and b) what that file looks like. That's definitely the biggest hurdle in getting started, because it's pretty much not documented anywhere.

Just keep in mind that every package in the release has to be published and on Atmosphere. The .meteor/versions file of an app is a particularly good place for finding all the necessary packages and versions that should go into the release.

After that, it's a matter of figuring out what you're willing to support, what you want to include, etc. Here is a partial Venn Diagram of what the Clinical Release is currently working on; and should give you a general idea of how we're going about the decision making process of what gets included.

For more discussion, see the topic on the Meteor Forums:  
https://forums.meteor.com/t/custom-meteor-release/13736/6

## Basic Usage
The idea is that a distro maintainer wants to run something like the following command:

```
meteor publish-release clinical.meteor.rc6.json
```

Which will then allow users of the distro to run this:

```
meteor run --release clinical:METEOR@1.1.3-rc6
```

## Release Manifest
A release manifest is similar to an NPM ``package.json`` file, in that it's primary concern is specify a list of Atmosphere packages, and providing a bit of metadata about that list of packages.  The basic format looks like this:
```
{
  "track":"distroname:METEOR",
  "version":"x.y.z",
  "recommended": false,
  "tool": "distroname:meteor-tool@x.y.z",
  "description": "Description of the Distro",
  "packages": {
    "accounts-base":"1.2.0",
    "accounts-password":"1.1.1",
    ...
  }
}
```

## Customizing the Meteor Tool
If you need to extend the meteor tool or the command line, you'll need to create and publish your own meteor-tool package. Ronen's documentation is the best out there for this process:

http://practicalmeteor.com/using-meteor-publish-release-to-extend-the-meteor-command-line-tool/1

It's easy to get a meteor helloworld command working, but after that, I felt it was easier to just create a separate node app to test out commands. Which is how StarryNight came about. It's something of a staging ground and scratchpad for commands before trying to put them into a version of the meteor-tool.

## Extracting a Release Manifest from .meteor/versions
StarryNight contains a small utility that parses an application's ``.meteor/versions`` file, and converts it into a Release Manifest.  

```
npm install -g starrynight
cd myapp
starrynight generate-release-json
```

If you don't wish to use StarryNight, simply copy the contents of your ``.meteor/versions`` file into the ``packages`` field of your manifest file.  Be sure to convert to JSON syntax and add colons and quotes.

## Displaying the Release Manifest for a Specific Release
```
meteor show --ejson METEOR@1.2.1
```

## Publishing a Release From Checkout
```
meteor publish-release --from-checkout
```

## Fetching the Latest Commits for Each Package in a Release
When building a custom release track, it's common to keep packages in the ``/packages`` directory as git submodules.  The following command allows you to fetch all of the latest commits for the submodules in your ``/packages`` directory at the same time.

```
git submodule foreach git pull origin master
```

