---
title: "Renaming"
slug: "renaming"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
- `git mv <source> <destination>`
- `git mv -f <source> <destination>`

## Parameters
| Parameter | Details |
|-----------|---------|
| `-f` or `--force` | Force renaming or moving of a file even if the target exists | 

## Rename Folders
To rename a folder from `oldName` to `newName`
    
    git mv directoryToFolder/oldName directoryToFolder/newName

Followed by `git commit` and/or `git push`

If this error occurs:

> fatal: renaming 'directoryToFolder/oldName' failed: Invalid argument

Use the following command:

    git mv directoryToFolder/oldName temp && git mv temp directoryToFolder/newName

## Renaming a local branch
You can rename branch in local repository using this command:

    git branch -m old_name new_name

## rename a local and the remote branch
the easiest way is to have the local branch checked out:

    git checkout old_branch

then rename the local branch, delete the old remote and set the new renamed branch as upstream:

    git branch -m new_branch
    git push origin :old_branch
    git push --set-upstream origin new_branch

