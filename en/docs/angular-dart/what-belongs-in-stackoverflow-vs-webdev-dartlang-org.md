---
title: "What belongs in stackoverflow vs. webdev.dartlang.org?"
slug: "what-belongs-in-stackoverflow-vs-webdevdartlangorg"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

What should go here, as opposed to webdev.dartlang.org/angular?

Eventually this page will have guidelines on what goes where.
(Is there a better place for this?)

**Please comment on https://github.com/dart-lang/site-webdev/issues/471.**

Background:
There are plenty of docs at (aka angulardart.org), and that site is publicly editable at https://github.com/dart-lang/site-webdev. However, the process for creating a new page is much more heavyweight than here.

## Setup for webdev
Getting set up to use webdev takes some time,
if you're doing anything more than tweaking a bit of text.
Here are the initial setup steps:

```
$ mkdir site  
$ cd site  
$ git clone git@github.com:dart-lang/site-webdev.git  
$ source ./scripts/env-set.sh  
$ ./scripts/install-dart-sdk.sh  
$ ./scripts/before-install.sh  
$ rvm install 2.3.0  
$ ./scripts/install.sh  
$ ./scripts/get-ng-repo.sh
```

For more details, see [one of our Travis
builds](https://travis-ci.org/dart-lang/site-webdev/builds/212315077).

