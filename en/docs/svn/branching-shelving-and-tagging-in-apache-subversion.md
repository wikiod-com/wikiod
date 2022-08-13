---
title: "Branching, shelving and tagging in Apache Subversion"
slug: "branching-shelving-and-tagging-in-apache-subversion"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Syntax
* svn copy [BRANCH-FROM-URL] [BRANCH-TO-URL] -m `<COMMIT-LOG-MESSAGE>`

* svn copy [^/PATH-TO-BRANCH-FROM] [^/PATH-TO-BRANCH-TO] -m `<COMMIT-LOG-MESSAGE>`

As you might have noticed, we use [`svn copy `][1] command to create branches, tags and shelves (we'll skip mentioning tags and shelves in the next paragraphs). This is the same command used to copy items in your working copy and into the repository.

`svn copy` is used for branching because, branch is technically a copy of the source you copy from. However, this is not an ordinary copy are familiar with when copying files on your local file system. Branches in Subversion are so called ["Cheap Copies"][2] that are similar to symlinks. Therefore, creating a new branch takes minimal time to complete and takes practically no space in the Subversion repository. Create branches and use them for any change you want regardless of the change's size and scope.

`svn copy` can be shortened to `svn cp` as Subversion has aliases for most commands.

  [1]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.copy.html
  [2]: http://svnbook.red-bean.com/en/1.8/svn.branchmerge.using.html#svn.branchmerge.using.create

## Creating a branch using direct URL to URL copy
Branching in Subversion is *very* simple. In the simplest form, creating a new branch requires you to run the command against the remote repository's URLs. For example, let's create a new branch out of the mainline trunk:

    svn copy https://svn.example.com/svn/MyRepo/MyProject/trunk https://svn.example.com/svn/MyRepo/MyProject/branches/MyNewBranch -m "Creating a new branch"

The new branch is ready and you can begin working with it. Check out a new working copy with the new branch or switch your existing working copy using [`svn switch`][1] command.


  [1]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.switch.html

## Using tags
"Tags" are a type of label that can be applied to a repository at a certain point in time.  They are frequently used to give human-readable names to important milestones so that they can be easily accessed later (for example, "version-1.2").

Creating a tag is exactly the same as creating a branch:

    svn copy -r 1234 ^/MyProject/trunk ^/MyProject/tags/version-1.2

In this specific case, the `-r 1234` argument was used to indicate that the tag should be created from revision 1234 of the trunk.

Subversion doesn't make any distinction between a tag and an ordinary branch.  The only difference is in how you decide to use them.  Traditionally, no commits are made to a tag once it has been created (to ensure that it remains an accurate "snapshot" of a past repository state).  Subversion doesn't enforce any special tag-related rules by default since different people can use tags differently.  A repository administrator can, however, set up access control scripts to enforce whatever rules their team has decided to use.

> In Windows, you need to use a double caret `^^`.

## Creating a branch through a working copy
When you interact with the remote central repository using your private local workspace -- the working copy -- you can use repository-relative URL instead of direct URL to URL copy to create a new branch:

    svn copy "^/MyProject/trunk" "^/MyProject/branches/MyNewBranch" -m "Creating a new branch"


## Switching a working copy to a different branch
An existing working copy can be quickly transformed to reflect the contents of a different branch in the same repository.  For example, you might have a working copy of the trunk and now need to work on a development branch.  Instead of checking out a completely new working copy (which can waste a lot of time and disk space), you can use the `svn switch` command to efficiently modify your existing working copy:

    svn switch ^/MyProject/branches/MyNewBranch

Your working copy will now reflect the contents of the branch instead of the trunk.

## Deleting a branch
Just run:

    svn delete https://svn.example.com/svn/MyRepo/MyProject/branches/MyNewBranch -m "Deleting no longer needed MyNewBranch"

Or, using the short URL:

    svn delete ^/branches/MyNewBranch -m "Deleting no longer needed MyNewBranch"

> - In Windows, you need to use ^^
> - You can always bring back a deleted branch by creating it again specifying the desired revision back then when the branch was alive (a deleted branch is just a branch that is not available in the HEAD revision). For example if branch was deleted at revision 101:
`svn copy https://svn.example.com/svn/MyRepo/branches/MyNewBranch@r100 https://svn.example.com/svn/MyRepo/MyProject/branches/resurrected-branch -m "Resurrected MyNewBranch from revision 100"`. See [Resurrecting Deleted Items](http://svnbook.red-bean.com/en/1.7/svn-book.html#svn.branchmerge.basicmerging.resurrect)

