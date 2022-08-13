---
title: "Creating directories"
slug: "creating-directories"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Manipulating directories from the command line

## Move all files not already in a directory into a self named directory
ll | grep ^- | awk -F"." '{print $2 "." $3}' | awk -F":" '{print $2}' | awk '{$1=""; print $0}' | cut -c2- | awk -F"." '{print "mkdir \""$1"\";mv \""$1"."$2"\" \""$1"\""}' > tmp;source tmp

