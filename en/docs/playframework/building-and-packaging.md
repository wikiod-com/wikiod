---
title: "Building and packaging"
slug: "building-and-packaging"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Syntax
* activator dist

## Add a directory to the distribution
To add for instance a directory `scripts` to the distribution package:

 1. Add to the project a folder **scripts** 
 2. On top of the build.sbt, add:
    
        import NativePackagerHelper._

 3. In build.sbt, add a mapping to the new directory:

        mappings in Universal ++= directory("scripts") 

 4. Build the distribution package with ***activator dist***. The newly created archive in `target/universal/` should contain the new directory.

