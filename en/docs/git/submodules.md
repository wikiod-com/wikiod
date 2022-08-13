---
title: "Submodules"
slug: "submodules"
draft: false
images: []
weight: 9877
type: docs
toc: true
---

## Cloning a Git repository having submodules
When you clone a repository that uses submodules, you'll need to initialize and update them.

    $ git clone --recursive https://github.com/username/repo.git

This will clone the referenced submodules and place them in the appropriate folders (including submodules within submodules). This is equivalent to running `git submodule update --init --recursive` immediately after the clone is finished.

## Updating a Submodule
A submodule references a specific commit in another repository. To check out the exact state that is referenced for all submodules, run

    git submodule update --recursive

Sometimes instead of using the state that is referenced you want to update to your local checkout to the latest state of that submodule on a remote. 
To check out all submodules to the latest state on the remote with a single command, you can use

    git submodule foreach git pull <remote> <branch>

or use the default `git pull` arguments

    git submodule foreach git pull

Note that this will just update your local working copy. Running `git status` will list the submodule directory as dirty if it changed because of this command. To update your repository to reference the new state instead, you have to commit the changes:

    git add <submodule_directory>
    git commit

There might be some changes you have that can have merge conflict if you use `git pull` so you can use `git pull --rebase` to rewind your changes to top, most of the time it decreases the chances of conflict. Also it pulls all the branches to local.

    git submodule foreach git pull --rebase

To checkout the latest state of a specific submodule, you can use :

    git submodule update --remote <submodule_directory>

## Adding a submodule
You can include another Git repository as a folder within your project, tracked by Git:

    $ git submodule add https://github.com/jquery/jquery.git

You should add and commit the new `.gitmodules` file; this tells Git what submodules should be cloned when `git submodule update` is run.

## Setting a submodule to follow a branch
A submodule is always checked out at a specific commit SHA1 (the "gitlink", special entry in the index of the parent repo)

But one can request to update that submodule to the latest commit of a branch of the submodule remote repo.

Rather than going in each submodule, doing a `git checkout abranch --track origin/abranch, git pull`, you can simply do (from the parent repo) a:

    git submodule update --remote --recursive

Since the SHA1 of the submodule would change, you would still need to follow that with:

    git add .
    git commit -m "update submodules"

That supposes the submodules were:

- either added with a branch to follow:

        git submodule -b abranch -- /url/of/submodule/repo

- or configured (for an existing submodule) to follow a branch:

        cd /path/to/parent/repo
        git config -f .gitmodules submodule.asubmodule.branch abranch
    

## Moving a submodule
<!-- if version [gt 1.8] -->
Run:
<pre><code>$ git mv <em>old/path/to/module</em> <em>new/path/to/module</em></pre></code>

<!-- end version if -->

<!-- if version [lte 1.8] -->
1. Edit `.gitmodules` and change the path of the submodule appropriately, and put it in the index with `git add .gitmodules`.
2. If needed, create the parent directory of the new location of the submodule (<code>mkdir -p *new/path/to*</code>).
3. Move all content from the old to the new directory (<code>mv -vi *old/path/to/module* *new/path/to/submodule*</code>).
4. Make sure Git tracks this directory (<code>git add *new/path*/to</code>).
5. Remove the old directory with <code>git rm --cached *old/path/to/module*</code>.
6. Move the directory <code>.git/modules/*old/path/to/module*</code> with all its content to <code>.git/modules/*new/path/to/module*</code>.
7. Edit the <code>.git/modules/*new/path/to*/config</code> file, make sure that worktree item points to the new locations, so in this example it should be <code>worktree = ../../../../../*old/path/to/module*</code>. Typically there should be two more `..` then directories in the direct path in that place.
. Edit the file <code>*new/path/to/module*/.git</code>, make sure that the path in it points to the correct new location inside the main project `.git` folder, so in this example <code>gitdir: ../../../.git/modules/*new/path/to/module*</code>.
   
   `git status` output looks like this afterwards:
   
        # On branch master
        # Changes to be committed:
        #   (use "git reset HEAD <file>..." to unstage)
        #
        #       modified:   .gitmodules
        #       renamed:    old/path/to/submodule -> new/path/to/submodule
        #

8. Finally, commit the changes.
<!-- end version if -->
---
This example from [Stack Overflow](http://stackoverflow.com/a/6310246), by [Axel Beckert](http://stackoverflow.com/users/793172)

## Removing a submodule
<!-- if version [gt 1.8] -->
You can remove a submodule (e.g. `the_submodule`) by calling:

    $ git submodule deinit the_submodule
    $ git rm the_submodule 

 - `git submodule deinit the_submodule` deletes `the_submodule`s' entry from .git/config. This excludes the_submodule from `git submodule update`, `git submodule sync` and `git submodule foreach` calls and deletes its local content [(source)](https://git-scm.com/docs/git-submodule#git-submodule-deinit). Also, this will not be shown as change in your parent repository. `git submodule init` and `git submodule update` will restore the submodule, again without commitable changes in your parent repository.

  - `git rm the_submodule` will remove the submodule from the work tree. The files will be gone as well as the submodules' entry in the `.gitmodules` file [(source)](https://git-scm.com/docs/git-rm#_submodules). If only `git rm the_submodule` (without prior `git submodule deinit the_submodule` is run, however, the submodules' entry in your .git/config file will remain.
<!-- end version if -->
<!-- if version [lt 1.8] -->
Taken from [here](http://stackoverflow.com/a/1260982/7598462):
  
 1. Delete the relevant section from the `.gitmodules` file.
 2. Stage the `.gitmodules` changes `git add .gitmodules`
 3. Delete the relevant section from `.git/config`.
 4. Run `git rm --cached path_to_submodule` (no trailing slash).
 5. Run `rm -rf .git/modules/path_to_submodule`
 6. Commit `git commit -m "Removed submodule <name>"`
 7. Delete the now untracked submodule files
 8. `rm -rf path_to_submodule`

<!-- end version if -->


