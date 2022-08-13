---
title: "Stashing"
slug: "stashing"
draft: false
images: []
weight: 9649
type: docs
toc: true
---

## Syntax
 - `git stash list [<options>]`
 - `git stash show [<stash>]`
 - `git stash drop [-q|--quiet] [<stash>]`
 - `git stash ( pop | apply ) [--index] [-q|--quiet] [<stash>]`
 - `git stash branch <branchname> [<stash>]`
 - `git stash [save [-p|--patch] [-k|--[no-]keep-index] [-q|--quiet] [-u|--include-untracked] [-a|--all] [<message>]]`
 - `git stash clear`
 - `git stash create [<message>]`
 - `git stash store [-m|--message <message>] [-q|--quiet] <commit>`

## Parameters
| Parameter | Details |
| --------- | ------- |
| show | Show the changes recorded in the stash as a diff between the stashed state and its original parent. When no \<stash\> is given, shows the latest one.|
| list | List the stashes that you currently have. Each stash is listed with its name (e.g. stash@{0} is the latest stash, stash@{1} is the one before, etc.), the name of the branch that was current when the stash was made, and a short description of the commit the stash was based on.  |
|pop|Remove a single stashed state from the stash list and apply it on top of the current working tree state.|
|apply|Like `pop`, but do not remove the state from the stash list.|
| clear | Remove all the stashed states. Note that those states will then be subject to pruning, and may be impossible to recover.  |
| drop | Remove a single stashed state from the stash list. When no \<stash> is given, it removes the latest one. i.e. stash@{0}, otherwise \<stash> must be a valid stash log reference of the form stash@{\<revision>}.  |
| create | Create a stash (which is a regular commit object) and return its object name, without storing it anywhere in the ref namespace. This is intended to be useful for scripts. It is probably not the command you want to use; see "save" above. |
| store | Store a given stash created via git stash create (which is a dangling merge commit) in the stash ref, updating the stash reflog. This is intended to be useful for scripts. It is probably not the command you want to use; see "save" above. |

Stashing allows us to have a clean working directory without losing any information. Then, it's possible to start working on something different and/or to switch branches.

## Create stash
Save the current state of working directory and the index (also known as the staging area) in a stack of stashes.

    git stash

To include all untracked files in the stash use the `--include-untracked` or `-u` flags.

    git stash --include-untracked

To include a message with your stash to make it more easily identifiable later

    git stash save "<whatever message>"

To leave the staging area in current state after stash use the `--keep-index` or `-k` flags.

    git stash --keep-index


## Apply and remove stash
To apply the last stash and remove it from the stack - type:

    git stash pop

To apply specific stash and remove it from the stack - type:

    git stash pop stash@{n}

## Apply stash without removing it
Applies the last stash without removing it from the stack

    git stash apply
Or a specific stash

    git stash apply stash@{n}

## Show stash
Shows the changes saved in the last stash

    git stash show
Or a specific stash

    git stash show stash@{n}
To show content of the changes saved for the specific stash

    git stash show -p stash@{n}

## What is Stashing?
When working on a project, you might be half-way through a feature branch change when a bug is raised against master. You're not ready to commit your code, but you also don't want to lose your changes. This is where `git stash` comes in handy.

Run `git status` on a branch to show your uncommitted changes:

    (master) $ git status
    On branch master
    Your branch is up-to-date with 'origin/master'.
    Changes not staged for commit:
      (use "git add <file>..." to update what will be committed)
      (use "git checkout -- <file>..." to discard changes in working directory)
    
        modified:   business/com/test/core/actions/Photo.c
    
    no changes added to commit (use "git add" and/or "git commit -a")

Then run `git stash` to save these changes to a stack:

    (master) $ git stash
    Saved working directory and index state WIP on master: 
    2f2a6e1 Merge pull request #1 from test/test-branch
    HEAD is now at 2f2a6e1 Merge pull request #1 from test/test-branch

If you have added files to your working directory these can be stashed as well. You just need to stage them first.

    (master) $ git stash
    Saved working directory and index state WIP on master:
    (master) $ git status
    On branch master
    Untracked files:
      (use "git add <file>..." to include in what will be committed)
    
            NewPhoto.c
    
    nothing added to commit but untracked files present (use "git add" to track)
    (master) $ git stage NewPhoto.c
    (master) $ git stash
    Saved working directory and index state WIP on master:
    (master) $ git status
    On branch master
    nothing to commit, working tree clean
    (master) $

Your working directory is now clean of any changes you made.  You can see this by re-running `git status`:

    (master) $ git status
    On branch master
    Your branch is up-to-date with 'origin/master'.
    nothing to commit, working directory clean

To apply the very last stash, run `git stash apply` (additionally, you can apply *and* remove the last stashed changed with `git stash pop`):

    (master) $ git stash apply
    On branch master
    Your branch is up-to-date with 'origin/master'.
    Changes not staged for commit:
      (use "git add <file>..." to update what will be committed)
      (use "git checkout -- <file>..." to discard changes in working directory)
    
        modified:   business/com/test/core/actions/Photo.c
    
    no changes added to commit (use "git add" and/or "git commit -a")

Note, however, that stashing does not remember the branch you were working on.  In the above examples, the user was stashing on **master**.  If they switch to the **dev** branch, **dev**, and run `git stash apply` the last stash is put on the **dev** branch.

    (master) $ git checkout -b dev
    Switched to a new branch 'dev'
    (dev) $ git stash apply
    On branch dev
    Changes not staged for commit:
      (use "git add <file>..." to update what will be committed)
      (use "git checkout -- <file>..." to discard changes in working directory)
    
        modified:   business/com/test/core/actions/Photo.c
    
    no changes added to commit (use "git add" and/or "git commit -a")

## Partial stash
If you would like to stash only *some* diffs in your working set, you can use a partial stash.

    git stash -p

And then interactively select which hunks to stash.

As of version 2.13.0 you can also avoid the interactive mode and create a partial stash with a pathspec using the new **push** keyword.

    git stash push -m "My partial stash" -- app.config

## List saved stashes
    git stash list

This will list all stashes in the stack in reverse chronological order.  
You will get a list that looks something like this:

    stash@{0}: WIP on master: 67a4e01 Merge tests into develop
    stash@{1}: WIP on master: 70f0d95 Add user role to localStorage on user login

You can refer to specific stash by its name, for example `stash@{1}`.

## Move your work in progress to another branch
If while working you realize you're on wrong branch and you haven't created any commits yet, you can easily move your work to correct branch using stashing:

    git stash
    git checkout correct-branch
    git stash pop

Remember `git stash pop` will apply the last stash and delete it from the stash list. To keep the stash in the list and only apply to some branch you can use:

    git stash apply

## Remove stash
Remove all stash

    git stash clear

Removes the last stash

    git stash drop
Or a specific stash

    git stash drop stash@{n}

## Apply part of a stash with checkout
You've made a stash and wish to checkout only some of the files in that stash.
```
git checkout stash@{0} -- myfile.txt
```

## Recovering earlier changes from stash


## Interactive Stashing


## Recover a dropped stash
If you have only just popped it and the terminal is still open, you will still have the hash value printed by `git stash pop` on screen:

    $ git stash pop
    [...]
    Dropped refs/stash@{0} (2ca03e22256be97f9e40f08e6d6773c7d41dbfd1)
(Note that git stash drop also produces the same line.)

Otherwise, you can find it using this:

    git fsck --no-reflog | awk '/dangling commit/ {print $3}'

This will show you all the commits at the tips of your commit graph which are no longer referenced from any branch or tag – every lost commit, including every stash commit you’ve ever created, will be somewhere in that graph.

The easiest way to find the stash commit you want is probably to pass that list to `gitk`:

    gitk --all $( git fsck --no-reflog | awk '/dangling commit/ {print $3}' )

This will launch a repository browser showing you *every single commit in the repository ever*, regardless of whether it is reachable or not.

You can replace `gitk` there with something like `git log --graph --oneline --decorate` if you prefer a nice graph on the console over a separate GUI app.

To spot stash commits, look for commit messages of this form:

&#160; &#160; &#160; &#160; WIP on <i>somebranch</i>: <i>commithash Some old commit message</i>

Once you know the hash of the commit you want, you can apply it as a stash:

<pre><code>git stash apply <i>$stash_hash</i></code></pre>

Or you can use the context menu in `gitk` to create branches for any unreachable commits you are interested in. After that, you can do whatever you want with them with all the normal tools. When you’re done, just blow those branches away again.

