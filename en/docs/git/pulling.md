---
title: "Pulling"
slug: "pulling"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

Unlike pushing with Git where your local changes are sent to the central repository's server, pulling with Git takes the current code on the server and 'pulls' it down from the repository's server to your local machine. This topic explains the process of pulling code from a repository using Git as well as the situations one might encounter while pulling different code into the local copy.

## Syntax
- git pull [options [&lt;repository> [&lt;refspec>...]]

## Parameters
| **Parameters** | **Details** |
| ---------- | ------- |
| `--quiet` | No text output |
| `-q` | shorthand for `--quiet` |
| `--verbose` | verbose text output. Passed to fetch and merge/rebase commands respectively. |
| `-v` | shorthand for `--verbose` |
| `--[no-]recurse-submodules[=yes`&#124;`on-demand`&#124;`no]`  | Fetch new commits for submodules? (Not that this is not a pull/checkout) |


`git pull` runs `git fetch` with the given parameters and calls `git merge` to merge the retrieved branch heads into the current branch. 

## Updating with local changes
When local changes are present, the `git pull` command aborts reporting :

> error: Your local changes to the following files would be overwritten
> by merge

In order to update (like svn update did with subversion), you can run :

    git stash
    git pull --rebase 
    git stash pop

A convenient way could be to define an alias using :

<!-- if version [lt 2.9] -->

    git config --global alias.up '!git stash && git pull --rebase && git stash pop'
<!-- end version if -->


<!-- if version [gte 2.9] -->

    git config --global alias.up 'pull --rebase --autostash'
<!-- end version if -->


Next you can simply use :

    git up

## Pull, overwrite local
    git fetch
    git reset --hard origin/master

**Beware:** While commits discarded using `reset --hard` can be recovered using `reflog` and `reset`, uncommitted changes are deleted forever.

Change `origin` and `master` to the remote and branch you want to forcibly pull to, respectively, if they are named differently.

## Pull code from remote
    git pull

## Keeping linear history when pulling
# Rebasing when pulling
If you are pulling in fresh commits from the remote repository and you have local changes on the current branch then git will automatically merge the remote version and your version. If you would like to reduce the number of merges on your branch you can tell git to [rebase][1] your commits on the remote version of the branch.

    git pull --rebase

___
# Making it the default behavior
To make this the default behavior for newly created branches, type the following command:

    git config branch.autosetuprebase always
To change the behavior of an existing branch, use this:

    git config branch.BRANCH_NAME.rebase true

And

    git pull --no-rebase

To perform a normal merging pull.
___
# Check if fast-forwardable
To only allow fast forwarding the local branch, you can use:

    git pull --ff-only
This will display an error when the local branch is not fast-forwardable, and needs to be either rebased or merged with upstream.

  [1]: https://www.wikiod.com/git/rebasing

## Pulling changes to a local repository
# Simple pull
When you are working on a remote repository (say, GitHub) with someone else, you will at some point want to share your changes with them. Once they have [pushed][1] their changes to a remote repository, you can retrieve those changes by _pulling_ from this repository.

    git pull
Will do it, in the majority of cases.

___
# Pull from a different remote or branch
You can pull changes from a different remote or branch by specifying their names

    git pull origin feature-A
Will pull the branch `feature-A` form `origin` into your local branch. Note that you can directly supply an URL instead of a remote name, and an object name such as a commit SHA instead of a branch name.

___
# Manual pull
To imitate the behavior of a git pull, you can use `git fetch` then `git merge`

    git fetch origin # retrieve objects and update refs from origin
    git merge origin/feature-A # actually perform the merge
This can give you more control, and allows you to inspect the remote branch before merging it. Indeed, after fetching, you can see the remote branches with `git branch -a`, and check them out with 

    git checkout -b local-branch-name origin/feature-A # checkout the remote branch
    # inspect the branch, make commits, squash, ammend or whatever
    git checkout merging-branches # moving to the destination branch
    git merge local-branch-name # performing the merge
This can be very handy when processing pull requests.

  [1]: https://www.wikiod.com/git/pushing

## Pull, "permission denied"
Some problems can occur if the `.git` folder has wrong permission. Fixing this problem by setting the owner of the complete `.git` folder. Sometimes it happen that another user pull and change the rights of the `.git` folder or files.

To fix the problem: 

    chown -R youruser:yourgroup .git/



