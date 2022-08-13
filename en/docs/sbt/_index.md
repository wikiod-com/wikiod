---
title : sbt Tutorial
slug : sbt-tutorial
weight : 9938
draft : false
images : []
type : docs
---

The Simple Build Tool (SBT for short) can be used to build Scala (or Java) project code. This includes managing code, dependencies, and resources that must be built, tested, and/or compiled to a `.jar` or other artifact. Custom tasks can be created to manage all of these processes.

A note on the name; SBT is sometimes referred to as the 'Scala Build Tool'. While this was not the original intent, it has come to be commonly used as well. SBT may be used to build any project on the JVM.

`.sbt` files, or 'SBT build definitions' are specially interpreted files, written in Scala, that are used by SBT to define a build. `.scala` build definitions may also be written and imported into an `.sbt` file.

Versions prior to `13.6` required that any `.sbt` file has each statement separated by a blank line. Without the blank line, the `.sbt` file will break.

A universal package exists in [ZIP][1] and [TGZ][2] formats.


  [1]: https://dl.bintray.com/sbt/native-packages/sbt/0.13.12/sbt-0.13.12.zip
  [2]: https://dl.bintray.com/sbt/native-packages/sbt/0.13.12/sbt-0.13.12.tgz

