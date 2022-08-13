---
title: "Setting up your preferred IDE"
slug: "setting-up-your-preferred-ide"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

## IntelliJ IDEA
Prerequisites
=====

1. Intellij IDEA installed (Community or Ultimate edition)
2. Scala Plugin installed in IntelliJ
2. A standard Play project, created for instance with Activator (`activator new [nameoftheproject] play-scala)`.

Opening the project
=======

1. Open IntelliJ IDEA
2. Go to menu `File` > `Open ...` > click the whole folder [nameoftheproject] > `OK`
3. A popup opens with a few options. The default values are good enough in most cases, and if you don't like them you can change them somewhere else later. Click `OK`
4. Intellij IDEA will think a bit, then propose another popup to select which modules to select in the project. There should be two modules `root` and `root-build` selected by default. Don't change anything and click `OK`.
5. IntelliJ will open the project. You can start viewing the files while IntelliJ keep thinking a bit as you should see in the status bar in the bottom, then it should finally be fully ready.


Running the applications from Intellij
=======

From there some people use the IDE just to view/edit the project, while using the `sbt` command line to compile/run/launch tests. Others prefer to launch those from within Intellij. It is required if you want to use the debug mode. Steps :

1. Menu `Run` > `Edit configurations...`
2. In the popup, click the `+` in the top left > Choose `Play 2 App` in the list
3. Name the configuration, for instance [nameofyourproject]. Leave the default options and hit `OK`.
4. From the `Run` menu, or the buttons in the UI, you can now `Run` or `Debug` using this configuration. `Run` will just launch the app, as if you did `sbt run` from the command line. `Debug` will do the same thing but allow you to place breakpoints in the code to interrupt the execution and analyze what's happening.

Auto-import option
=======

This is an option global to the project, that is available at creation time and afterwards can be changed in the menu `Intellij IDEA` > `Preferences` > `Build, Execution, Deployment` > `Build tools` > `SBT` > `Project-level settings` > `Use auto-import`.

This option has nothing to do with the `import` statements in the Scala code. It dictates what Intellij IDEA should do when you edit the `build.sbt` file. If auto-import is activated, Intellij IDEA will parse the new build file immediately and refresh the project configuration automatically. It gets annoying quickly as this operation is expensive and tends to slow Intellij when you're still working on the build file. When auto-import is desactivated, you have to indicate manually to Intellij that you edited the `build.sbt` and would like the project configuration to be refreshed. In most cases a temporary popup will appear to ask you if you would like to do so. Otherwise go to the SBT panel in the UI, and click the blue circling arrows sign to force the refresh.







## Eclipse as Play IDE - Java, Play 2.4, 2.5
Introduction
============
Play has several plugins for different IDE-s. The <b>eclipse</b> plugin allows to transform a Play application into a working eclipse project with the command *activator eclipse*. Eclipse plugin may be set per project or globally per <b>sbt</b> user. It depends on team work, which approach should be used. If the whole team is using eclipse IDE, plugin may be set on a project level.
You need to download eclipse version supporting Scala and Java 8: <b>luna</b> or <b>mars</b> - from <a href="http://scala-ide.org/download/sdk.html">http://scala-ide.org/download/sdk.html</a>.

Setting eclipse IDE per project
===============================
To import Play application into eclipse:
1. Add eclipse plugin into *project/plugins.sbt*:

      
    //Support Play in Eclipse
    addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "4.0.0")
2. Add into *build.sbt* a flag that forces compilation to happen when the eclipse command is run:


    EclipseKeys.preTasks := Seq(compile in Compile)
3. Make sure, that a user repository path in the file {user root}\.sbt\repositories has the proper format. The proper values for properties *activator-launcher-local* and *activator-local* should have at least three slashes like in the example:


    activator-local: file:////${activator.local.repository-C:/Play-2.5.3/activator-dist-1.3.10//repository}, [organization]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext]
    activator-launcher-local: file:////${activator.local.repository-${activator.home-${user.home}/.activator}/repository}, [organization]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext]
4. Compile the application:


    activator compile
5. Prepare an eclipse project for the new application with:


    activator eclipse
Now the project is ready to be imported into eclipse via <b>Existing Projects into Workspace</b>.

How to attach <b>Play</b> source to eclipse
-------------------------------------------
1. Add to the *build.sbt*:


    EclipseKeys.withSource := true
2. Compile the project


Setting eclipse IDE globally
============================
Add the <b>sbt</b> user setting:
1. Create under the user root directory a folder *.sbt\0.13\plugins* and a file *plugins.sbt*. For example for Windows user <b>asch</b>:


    c:\asch\.sbt\0.13\plugins\plugins.sbt
2. Add eclipse plugin into *plugins.sbt*:


    //Support Play in Eclipse
    addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "4.0.0")
3. Create in user *.sbt* directory a file *sbteclipse.sbt*. For example for Windows</b> user <b>asch</b>:


    c:\asch\.sbt\0.13\sbteclipse.sbt
4. Put into *sbteclipse.sbt* a flag that forces compilation to happen when the **activator eclipse** command is run:


    import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys
    EclipseKeys.preTasks := Seq(compile in Compile)
5. Add optionally other **EclipseKeys** settings.

Debugging from eclipse
======================
To debug, start the application with the default port 9999:

    activator -jvm-debug run
or with the different port:

    activator -jvm-debug [port] run
In eclipse:
1. Right-click on the project and select <b>Debug As</b>, <b>Debug Configurations</b>. 
2. In the <b>Debug Configurations</b> dialog, right-click on <b>Remote Java Application</b> and select <b>New</b>. 
3. Change Port to relevant (9999 if the default debug port was used) and click <b>Apply</b>. 

From now on you can click on <b>Debug</b> to connect to the running application. Stopping the debugging session will not stop the server.


## Eclipse IDE
Prerequisites
-------------

 1. Java8 (1.8.0_91) 
 2. Eclipse neon (JavaScript and Web Developer)
 3. Play Framework 2.5.4

Installing Scala in Eclipse
---------------

 1. Launch the Eclipse
 2. Open `Help`  > `Eclipse Marketplace`
 3. Type `Scala` in `Find`
 4. Install Scala IDE

Setup sbteclipse
--------------------------

 1. Open play project `.\project\ plugins.sbt`
 2. Add following command in `plugins.sbt` to convert eclipse project

> addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" %
> "4.0.0")

3. Open command and go to play project e.g. `cd C:\play\play-scala`. Type following at command line

> activator eclipse

Importing project
--------------

1. Go to menu `File` > `Import`in Eclipse
2. Select `Existing Projects into Workspace`
3. Select root directory

Now your project is ready to view and edit at Eclipse IDE.

