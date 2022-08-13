---
title: "Getting started with Codename One"
slug: "getting-started-with-codename-one"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation & Setup
# Installation

## Installing Codename One In NetBeans

These instructions assume you have downloaded a recent version of NetBeans (at this time 8.x), installed and launched it.

- Select the Tools->Plugins menu option
[![Tools->Plugin menu option][1]][1]

- Select the Available Plugins Tab

- Check The CodenameOne Plugin
[![Netbeans Plugin Install Wizard][2]][2]

- click the `install` button below. Follow the Wizard instructions to install the plugin
[![Netbeans Plugin Install Wizard step 2][3]][3]

## Installing Codename One In Eclipse

Startup Eclipse and click `Help`->`Install New Software`. You should get this dialog

[![Eclipse Install New Software][4]][4]

Paste `https://www.codenameone.com/files/eclipse/site.xml` for the location to Work with and press Enter.

Select the entries & follow the wizard to install
[![Eclipse Install Wizard select entries][6]][6]

## Installing Codename One In IntelliJ IDEA

Download & install IntelliJ/IDEA. **Notice that Android Studio will not work.**

Install the plugin using The Plugin Center

Use the search functionality in the plugin center to find and install the Codename One plugin.


  [1]: http://i.stack.imgur.com/Pnw9n.png
  [2]: http://i.stack.imgur.com/17BLM.png
  [3]: http://i.stack.imgur.com/NakDU.png
  [4]: http://i.stack.imgur.com/zdPEe.png
  [5]: http://i.stack.imgur.com/DfgaQ.png
  [6]: http://i.stack.imgur.com/AuCxh.png

## What is Codename One & How Does it Work?
Codename One is a set of tools for mobile application development that derive a great deal of its architecture from Java.

Codename One's mission statement is:

> Unify the complex and fragmented task of mobile device programming
> into a single set of tools, APIs & services. As a result create a more
> manageable approach to mobile application development without
> sacrificing the power/control given to developers.

This effectively means bringing that old "Write Once Run Anywhere" (WORA) Java mantra to mobile devices without "dumbing it down" to the lowest common denominator.

# How Does Codename One Work?

Codename One unifies several technologies and concepts into a single facade:

- API - abstracts the differences between the various devices.
- Plugin - the only piece of software installed on client machines, it includes the following features:
    * IDE integration - preferences, completion, the ability to send a native build
    * Simulator - native device simulator that runs locally and allows debugging the application
    * Designer/GUI Builder - high level tools
- Build Servers - The build servers accept native device builds sent by the plugin and convert the binaries (JAR's, not sources) to native applications as explained below.
- Cloud Servers - The cloud servers provide features such as push notification, cloud logging etc.

[![The Codename One tool-chain][1]][1]

## Why Build Servers?

The build servers allow building native iOS Apps without a Mac and native Windows apps without a Windows machine. They remove the need to install/update complex toolchains and simplify the process of building a native app to a right click.

E.g.: Since building native iOS applications requires a Mac OS X machine with a recent version of xcode Codename One maintains such machines in the cloud. When developers send an iOS build such a Mac will be used to generate C source code using [ParparVM][2] and it will then compile the C source code using xcode & sign the resulting binary using xcode. You can install the binary to your device or build a distribution binary for the appstore. Since C code is generated it also means that your app will be "future proof" in a case of changes from Apple. You can also inject Objective-C native code into the app while keeping it 100% portable thanks to the "native interfaces" capability of Codename One.

Subscribers can receive the C source code back using the include sources feature of Codename One and use those sources for benchmarking, debugging on devices etc.

The same is true for most other platforms. For the Android, J2ME & Blackberry the standard Java code is executed as is.

Java 8 syntax is supported thru [retrolambda][3] installed on the Codename One servers. This is used to convert bytecode seamlessly down to Java 5 syntax levels. Java 5 syntax is translated to the JDK 1.3 cldc subset on J2ME/Blackberry to provide those language capabilities and API's across all devices. This is done using a server based bytecode processor based on retroweaver and a great deal of custom code. Notice that this architecture is transparent to developers as the build servers abstract most of the painful differences between devices.

## Why ParparVM

On iOS, Codename One uses [ParparVM][2] which translates Java bytecode to C code and boasts a non-blocking GC as well as 64 bit/bitcode support. This VM is fully open source in the [Codename One git repository][4]. In the past Codename One used [XMLVM][5] to generate native code in a very similar way but the XMLVM solution was too generic for the needs of Codename One. [ParparVM][2] boasts a unique architecture of translating code to C (similarly to XMLVM), because of that Codename One is the only solution of its kind that can **guarantee** future iOS compatibility since the officially supported iOS toolchain is always used instead of undocumented behaviors.

> **NOTE:** XMLVM could guarantee that in theory but it is no longer maintained.

The key advantages of ParparVM over other approaches are:

- Truly native - since code is translated to C rather than directly to ARM or LLVM code the app is "more native". It uses the official tools and approaches from Apple and can benefit from their advancements e.g. latest bitcode or profiling capabilities.

- Smaller class library - ParparVM includes a very small segment of the full JavaAPI's resulting in final binaries that are smaller than the alternatives by orders of magnitude. This maps directly to performance and memory overhead.

- Simple & extensible - to work with ParparVM you need a basic understanding of C. This is crucial for the fast moving world of mobile development, as Apple changes things left and right we need a more agile VM.

## Windows Phone/UWP
Codename One has 2 major Windows VM ports and 3 or 4 rendering pipelines within those ports.

The old Windows Phone port used XMLVM to translate the Java bytecode to C#. Notice that the XMLVM backend that translates to C# is very different from the one that was used in the past to translates code for iOS.

Codename One now targets UWP by leveraging a modified version of iKVM to build native Windows Universal Applications. 

## JavaScript Port

The JavaScript port of Codename One is based on the amazing work of the [TeaVM project][6]. The team behind TeaVM effectively built a JVM that translates Java bytecode into JavaScript source code while maintaining threading semantics using a very imaginative approach.

The JavaScript port allows unmodified Codename One applications to run within a desktop or mobile browser. The port itself is based on the HTML5 Canvas API to provide a pixel perfect implementation of the Codename One API's.

> **NOTE:** The JavaScript port is only available for Enterprise grade subscribers of Codename One.

## Desktop, Android, RIM & J2ME

The other ports of Codename One use the VM's available on the host machines/environments to execute the runtime. https://github.com/orfjackal/retrolambda[Retrolambda] is used to provide Java 8 language features in a portable way, for older devices retroweaver is used to bring Java 5 features.

The Android port uses the native Android tools including the gradle build environment in the latest versions.

The desktop port creates a standard JavaSE application which is packaged with the JRE and an installer.

> **NOTE:** The Desktop port is only available to pro grade subscribers of Codename One.

## Lightweight Components

What makes Codename One stand out is the approach it takes to UI where it uses a "lightweight architecture" thus allowing the UI to work seamlessly across all platforms. As a result most of the UI is developed in Java and is thus remarkably portable and debuggable. The lightweight architecture still includes the ability to embed "heavyweight" widgets into place among the "lightweights".

### Lightweight Architecture Origin

Lightweight components date back to Smalltalk frameworks, this notion was popularized in the Java world by Swing. Swing was the main source of inspiration to Codename One's predecessor LWUIT. Many frameworks took this approach over the years including JavaFX & most recently Ionic in the JavaScript world.

### In Codename One

A Lightweight component is a component that is written entirely in Java, it draws its own interface and handles its own events/states. This has huge portability advantages since the same code executes on all platforms, but it carries many additional advantages.

Lightweight components are infinitely customizable by using standard inheritance and overriding paint/event handling. Since a lightweight component is written entirely in Java, developers can preview the application accurately in the simulators & GUI builder. This avoids many common pitfalls of other WORA solutions where platform specific behavior foiled any saved effort. Hence all the effort saved in coding was lost in debugging esoteric device only oddities.

Codename One achieves fast performance by drawing using the native gaming API's of most platforms e.g. OpenGL ES on iOS.

## Versions In Codename One

One of the confusing things about Codename One is the versions. Since Codename One is a SaaS product versioning isn't as simple as a 2.x or 3.x moniker. However, to conform to this convention Codename One does make versioned releases which contribute to the general confusion.

When a version of Codename One is released the version number refers to the libraries at the time of the release. These libraries are then frozen and are made available to developers who use the [Versioned Builds][7] feature. The plugin, which includes the designer as well as all development that is unrelated to versioned builds continues with its regular updates immediately after release. The same is true for the build servers that move directly to their standard update cycle.


  [1]: http://i.stack.imgur.com/PZeI9.png
  [2]: https://github.com/codenameone/CodenameOne/tree/master/vm
  [3]: https://github.com/orfjackal/retrolambda
  [4]: https://github.com/codenameone/CodenameOne/
  [5]: http://www.xmlvm.org/
  [6]: http://teavm.org
  [7]: https://www.codenameone.com/how-do-i---get-repeatable-builds-build-against-a-consistent-version-of-codename-one-use-the-versioning-feature.html

