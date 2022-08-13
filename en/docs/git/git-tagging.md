---
title: "Git Tagging"
slug: "git-tagging"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Like most Version Control Systems (VCSs), `Git` has the ability to `tag` specific points in history as being important. Typically people use this functionality to mark release points (`v1.0`, and so on).

## Syntax
- git tag [-a | -s | -u < keyid >] [-f] [-m < msg > | -F < file >]
    < tagname > [< commit > | < object >]

- git tag -d <  tagname​  >

- git tag [-n[< num >]] -l [--contains < commit >] [--contains < commit >]
    [--points-at < object >] [--column[=< options >] | --no-column]
    [--create-reflog] [--sort=< key >] [--format=< format >]
    [--[no-]merged [< commit >]] [< pattern >…​]

- git tag -v [--format=< format >] < tagname >…​

## Listing all available tags
Using the command `git tag` lists out all available tags:

    $ git tag
    <output follows>
    v0.1
    v1.3


> **Note**: the `tags` are output in an **alphabetical** order.

One may also `search` for available `tags`:

    $ git tag -l "v1.8.5*"
    <output follows>
    v1.8.5
    v1.8.5-rc0
    v1.8.5-rc1
    v1.8.5-rc2
    v1.8.5-rc3
    v1.8.5.1
    v1.8.5.2
    v1.8.5.3
    v1.8.5.4
    v1.8.5.5

## Create and push tag(s) in GIT
**Create a tag:**

- To create a tag on your current branch:

      git tag < tagname >

  This will create a local `tag` with the current state of the branch you are on.

- To create a tag with some commit:

      git tag tag-name commit-identifier

  This will create a local `tag` with the commit-identifier of the branch you are on.

**Push a commit in GIT:**

- Push an individual tag:

      git push origin tag-name

- Push all the tags at once

      git push origin --tags



