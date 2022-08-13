---
title: "List of commands"
slug: "list-of-commands"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Commands for preparing commits
* **add**:          add the specified files on the next commit
* **addremove**:    add all new files, delete all missing files
* **backout**:      reverse effect of earlier changeset
* **commit, ci**:   commit the specified files or all outstanding changes
* **copy, cp**:     mark files as copied for the next commit
* **forget**:       forget the specified files on the next commit
* **merge**:        merge another revision into working directory
* **remove, rm**:   remove the specified files on the next commit
* **rename, move, mv**: rename files; equivalent of copy + remove
* **resolve**:      redo merges or set/view the merge status of files
* **revert**:       restore files to their freshly-checked-out state


## Inspecting the history
* **annotate, blame**: show changeset information by line for each file
* **bisect**: subdivision search of changesets
* **cat**: output the current or given revision of files
* **diff**: diff repository (or selected files)
* **grep**: search for a pattern in specified files and revisions
* **log, history**: show revision history of entire repository or files

## Exchanging changesets with remote repos
* **archive**: create an unversioned archive of a repository revision
* **bundle**: create a changegroup file
* **clone**: make a copy of an existing repository
* **export**: dump the header and diffs for one or more changesets
* **graft**: copy changes from other branches onto the current branch
* **incoming**: show new changesets found in source
* **import, patch**: import an ordered set of patches
* **init**: create a new repository in the given directory
* **outgoing**: show changesets not found in the destination
* **phase**: set or show the current phase name
* **pull**: pull changes from the specified source
* **push**: push changes to the specified destination
* **recover**: roll back an interrupted transaction
* **rollback**: roll back the last transaction (DANGEROUS) (DEPRECATED)
* **serve**: start stand-alone webserver
* **unbundle**: apply one or more changegroup files


## Status: where are you now?
* **bookmarks, bookmark**: create a new bookmark or list existing bookmarks
* **branch**: set or show the current branch name
* **branches**: list repository named branches
* **config, showconfig, debugconfig**: show combined config settings from all hgrc files
* **files**: list tracked files
* **help**: show help for a given topic or a help overview
* **identify, id**: identify the working directory or specified revision
* **incoming, in**: show new changesets found in source
* **locate**: locate files matching specific patterns (DEPRECATED)
* **manifest**: output the current or given revision of the project manifest
* **outgoing, out**: show changesets not found in the destination
* **parents**: show the parents of the working directory or revision (DEPRECATED)
* **paths**: show aliases for remote repositories
* **phase**: set or show the current phase name
* **root**: print the root (top) of the current working directory
* **status, st**: show changed files in the working directory
* **summary, sum**: summarize working directory state
* **tags**: list repository tags
* **tip**: show the tip revision (DEPRECATED)
* **verify**: verify the integrity of the repository
* **version**: output version and copyright information


## Workflow: branches, tags, and moving about
* **bookmarks, bookmark**: create a new bookmark or list existing bookmarks
* **branch**: set or show the current branch name
* **tag**: add one or more tags for the current or given revision
* **update, up, checkout, co**: update working directory (or switch revisions)

