---
title: "Common operations"
slug: "common-operations"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Using the bisect command to find a bug
The `bisect` command helps you to track down the changeset that introduced a bug.

- Reset the bisect state and mark the current revision as bad (it contains the bug!)

       hg bisect --reset
       hg bisect --bad

- Go back to a point where you think the bug isn't present

       hg update -r -200

- Now you've to test the software and if your assumption was right (bug not present), mark the revision as good:

       hg bisect --good

   > Testing changeset 800:12ab34cd56ef (x changesets remaining, ~y tests)

- Mercurial updates the current revision (somewhere in the middle between the bad and good changeset)

- Test again the software and mark appropriately the current revision. E.g.

       hg bisect --good

   > Testing changeset 900:21ba43dc65fe (x changesets remaining, ~y tests)

- ...
- Continue until Mercurial has narrowed the search down to a single changeset:

       hg bisect --bad

  > The first bad revision is:
  >
  > changeset: 987:1234bad99889
  >
  > user: John Doe <____@gmail.com>
  >
  > date: Jul 28 16:00:00 2016

The `hg bisect` command uses its knowledge of your project's revision history to perform a search in time proportional to the logarithm of the number of changesets to check and has no problems dealing with branches, merges or multiple heads.

Sometimes you have an idea of the incriminated files and you can give an hint to Mercurial:

    hg bisect --skip "!( file('path:foo') & file('path:bar') )"

This skips all revisions that do not touch directories `foo` or `bar`.


## Using the revert command to discard unwanted changes.
The `revert` command allows discarding unwanted uncommitted changes.

 - Reverting changes to a single file.


    hg revert example.c

 - Reverting all changes.

This will discard **all** changes not just the current directory.

    hg revert --all

hg will output which files were reverted.

  > reverting example.c
  >
  > reverting mydir\example.cpp 
  >
  > forgetting file.txt

Backup files are produced for discarded changes to previously committed files, in the form `filename.orig`


