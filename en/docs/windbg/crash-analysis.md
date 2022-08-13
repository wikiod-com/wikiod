---
title: "Crash analysis"
slug: "crash-analysis"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Basic user mode crash analysis
`.exr -1` gives you details about the last exception thrown.

`!analyze -v` usually does a good job as well.

For .NET, the command `!pe` of the SOS extension shows details about the .NET exception that was thrown.

