---
title: "Comments"
slug: "comments"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## End of Line
Any text following a `"` character on the same line is commented out:

    DATA ls_booking TYPE flightb. " Commented text

## Full Line
The `*` character comments out an entire line. The `*` must be the first character in the line.

    * DATA ls_booking TYPE flightb. Nothing on this line will be executed.

