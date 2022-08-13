---
title: "Tidying up your local and remote repository"
slug: "tidying-up-your-local-and-remote-repository"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Delete local branches that have been deleted on the remote
To remote tracking between local and deleted remote branches use

    git fetch -p

you can then use 

    git branch -vv

to see which branches are no longer being tracked. 

Branches that are no longer being tracked will be in the form below, containing 'gone' 

     branch               12345e6 [origin/branch: gone] Fixed bug

you can then use a combination of the above commands, looking for where 'git branch -vv' returns 'gone' then using '-d' to delete the branches

    git fetch -p && git branch -vv | awk '/: gone]/{print $1}' | xargs git branch -d

