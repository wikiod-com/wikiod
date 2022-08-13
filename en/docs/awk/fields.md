---
title: "Fields"
slug: "fields"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Looping trough fields
    awk '{ for(f=1; f<=NF; f++) { print $f; } }' file

