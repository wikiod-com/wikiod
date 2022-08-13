---
title: "Working with Remotes"
slug: "working-with-remotes"
draft: false
images: []
weight: 9890
type: docs
toc: true
---

## Syntax
 - `git remote [-v | --verbose]`
 - `git remote add [-t <branch>] [-m <master>] [-f] [--[no-]tags] [--mirror=<fetch|push>] <name> <url>`
 - `git remote rename <old> <new>`
 - `git remote remove <name>`
 - `git remote set-head <name> (-a | --auto | -d | --delete | <branch>)`
 - `git remote set-branches [--add] <name> <branch>…​`
 - `git remote get-url [--push] [--all] <name>`
 - `git remote set-url [--push] <name> <newurl> [<oldurl>]`
 - `git remote set-url --add [--push] <name> <newurl>`
 - `git remote set-url --delete [--push] <name> <url>`
 - `git remote [-v | --verbose] show [-n] <name>…​`
 - `git remote prune [-n | --dry-run] <name>…​`
 - `git remote [-v | --verbose] update [-p | --prune] [(<group> | <remote>)…​]`

## Deleting a Remote Branch


## Changing Git Remote URL
Check existing remote

    git remote -v 
    # origin https://github.com/username/repo.git (fetch)
    # origin https://github.com/usernam/repo.git (push)

Changing repository URL

    git remote set-url origin https://github.com/username/repo2.git
    # Change the 'origin' remote's URL

Verify new remote URL

    git remote -v
    # origin  https://github.com/username/repo2.git (fetch)
    # origin  https://github.com/username/repo2.git (push)

## Updating from Upstream Repository
Assuming you set the upstream (as in the "setting an upstream repository")

    git fetch remote-name
    git merge remote-name/branch-name

The `pull` command combines a `fetch` and a `merge`.

    git pull

The `pull` with `--rebase` flag command combines a `fetch` and a `rebase` instead of `merge`.

    git pull --rebase remote-name branch-name


## ls-remote


## Removing Local Copies of Deleted Remote Branches


## List Existing Remotes
List all the existing remotes associated with this repository:

    git remote

List all the existing remotes associated with this repository in detail including the `fetch` and `push` URLs:

    git remote --verbose

or simply

    git remote -v


## Adding a New Remote Repository
    git remote add upstream git-repository-url

Adds remote git repository represented by `git-repository-url` as new remote named `upstream` to the git repository

## Getting Started


## Set Upstream on a New Branch


## Show information about a Specific Remote
Output some information about a known remote: `origin`

    git remote show origin

Print just the remote's URL:

    git config --get remote.origin.url

With 2.7+, it is also possible to do, which is arguably better than the above one that uses the `config` command.

    git remote get-url origin

## Changing a Remote Repository
To change the URL of the repository you want your remote to point to, you can use the `set-url` option, like so:

    git remote set-url <remote_name> <remote_repository_url>

Example:

    git remote set-url heroku https://git.heroku.com/fictional-remote-repository.git

## Renaming a Remote
To rename remote, use command `git remote rename`

The `git remote rename` command takes two arguments:

 - An existing remote name, for example : **origin**
 - A new name for the remote, for example : **destination**

Get existing remote name

    git remote
    # origin

Check existing remote with URL

    git remote -v 
    # origin https://github.com/username/repo.git (fetch)
    # origin https://github.com/usernam/repo.git (push)

Rename remote 

     git remote rename origin destination
     # Change remote name from 'origin' to 'destination'

Verify new name
 
    git remote -v 
    # destination https://github.com/username/repo.git (fetch)
    # destination https://github.com/usernam/repo.git (push)

**=== Posible Errors ===**

1. Could not rename config section 'remote.[old name]' to 'remote.[new name]'
 
     This error means that the remote you tried the old remote name (**origin**) doesn't exist.


2. Remote [new name] already exists.
      
      Error message is self explanatory. 



## Set the URL for a Specific Remote
You can change the url of an existing remote by the command

    git remote set-url remote-name url 

## Get the URL for a Specific Remote
You can obtain the url for an existing remote by using the command 

`git remote get-url <name>` 

By default, this will be 

`git remote get-url origin`

