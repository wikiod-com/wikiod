---
title: "Exporting"
slug: "exporting"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Building a .jar
Eventually, when you're ready to release a version of your code to production, you'll need a [`.jar`][1] file to distribute. Intellij makes building JARs quite easy.

First, navigate to `File -> Project Structure` and click on `Artifacts`:
[![Artifacts][2]][2]

Click on the `+` button, and select `JAR -> From modules with dependencies`:
[![enter image description here][3]][3]

Select your module from the dropdown list, and the main file (this is the file that contains your `public static void main()` method):
[![enter image description here][4]][4]

Click `OK`, verify that all the information regarding dependencies is correct, and click `OK` to finish setting up the artifact.
[![enter image description here][5]][5]

We're not done yet! We've only told Intellij how to build the artifact, now we actually need to make the `.jar`.

Simply click `Build -> Build Artifacts`, and click `Build` on the popup menu:
[![enter image description here][6]][6]

The `jar` will be found in `build -> classes -> artifacts`


  [1]:https://www.wikiod.com/java/getting-started-with-java-language
  [2]: http://i.stack.imgur.com/vX0E4.png
  [3]: http://i.stack.imgur.com/j8uzf.png
  [4]: http://i.stack.imgur.com/2zqtv.png
  [5]: http://i.stack.imgur.com/3pLbc.png
  [6]: http://i.stack.imgur.com/Vbglw.png

