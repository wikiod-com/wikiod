---
title: "Display commit history graphically with Gitk"
slug: "display-commit-history-graphically-with-gitk"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Display commit history for one file
`gitk path/to/myfile`

## Display all commits between two commits
Let's say you have two commits `d9e1db9` and `5651067` and want to see what happened between them. 
`d9e1db9` is the oldest ancestor and `5651067` is the final descendant in the chain of commits.

`gitk --ancestry-path d9e1db9 5651067`


## Display commits since version tag
If you have the version tag `v2.3` you can display all commits since that tag.

`gitk v2.3..`

