---
title: "Subtrees"
slug: "subtrees"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Syntax
- `git subtree add   -P <prefix> <commit>`
- `git subtree add   -P <prefix> <repository> <ref>`
- `git subtree pull  -P <prefix> <repository> <ref>`
- `git subtree push  -P <prefix> <repository> <ref>`
- `git subtree merge -P <prefix> <commit>`
- `git subtree split -P <prefix> [OPTIONS] [<commit>]`

This is an alternative to using a [`submodule`](https://git-scm.com/docs/git-submodule)

## Create, Pull, and Backport Subtree
# Create Subtree

Add a new remote called `plugin` pointing to the plugin's repository:

    git remote add plugin https://path.to/remotes/plugin.git

Then Create a subtree specifying the new folder prefix `plugins/demo`. `plugin` is the remote name, and `master` refers to the master branch on the subtree's repository:

    git subtree add --prefix=plugins/demo plugin master

# Pull Subtree Updates

Pull normal commits made in plugin:

    git subtree pull --prefix=plugins/demo plugin master

# Backport Subtree Updates

1. Specify commits made in superproject to be backported:

       git commit -am "new changes to be backported"

2. Checkout new branch for merging, set to track subtree repository:

       git checkout -b backport plugin/master

3. Cherry-pick backports:

       git cherry-pick -x --strategy=subtree master

4. Push changes back to plugin source:

       git push plugin backport:master

