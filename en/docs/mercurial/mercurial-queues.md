---
title: "Mercurial Queues"
slug: "mercurial-queues"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
 - hg qnew -m "My commit message" myPatch
 - hg qpop
 - hg qpush
 - hg qrefresh -m "My new commit message"
 - hg qapplied
 - hg qseries
 - hg qfinish
 - hg qdelete myPatch
 - hg qfold myPatch
 - hg qqueue --list
 - hg qqueue --create myNewQueue
 - hg qqueue --delete myNewQueue


## Enable Extension
Edit **Mercurial.ini** (Windows) or **.hgrc** (Linux/OSX):

    [extenstions]
    mq =

## Create and Update Patches
Create new patches with `hg qnew patch-name` and then update them with new changes with `hg qrefresh`:

    hg qnew myFirstPatch  // Creates a new patch called "myFirstPatch"
    ...                   // Edit some files
    hg qrefresh           // Updates "myFirstPatch" with changes
    hg qnew mySecondPatch // Creates a new patch called "mySecondPatch" on top of "myFirstPatch"
    ...                   // Edid some files
    hg qrefresh           // Updates "mySecondPatch" with changes

Create new patch with commit message:

    hg qnew -m "My first patch" myFirstPatch

Update commit message of current patch:

    hg qrefresh -m "My new message"

Update commit message (multiline) of current patch:

    hg qrefresh -e

## Push and Pop Patches
Pushing a patch applies the patch to the repository while poping a patch unapplies the patch from the repository.

Pop the current patch off the queue with `hg qpop` and push it back onto the queue with `hg qpush`:

    hg qpop
    hg qpush

Pop all patches:

    hg qpop -a

PUsh all patches:

    hg qpush -a

## Finish Patches
Finishing patches makes them permanent changesets.

Finish **first** applied patch in the queue:

    hg qfinish

Finish **all** applied patches in the queue:

    hg qfinish -a

## Rebase
To rebase with latest changesets in upstream repository:

    hg qpop -a    // Pop all patches
    hg pull -u    // Pull in changesets from upstream repo and update
    hg qpush -a   // Push all patches on top of new changesets

If there are any conflicts you will be forced to merge your patches.

## Fold/Combine Patches
To fold (combine) two patches:

    hg qnew firstPatch   // Create first patch
    hg qnew secondPatch  // Create second patch
    hg qpop              // Pop secondPatch
    hg qfold secondPatch // Fold secondPatch with firstPatch

## Multiples Queues
More than one queue may be created. Each queue can be thought of as a separate branch.

    hg qqueue --create foo // Create a new queue called "foo"
    hg qqueue --list       // List all queues
    hg qqueue --active     // Print name of active queue
    hg qqueue --rename bar // Rename active queue "foo" to "bar"
    hg qqueue --delete bar // delete queue "bar"

Create two queues and switch between them:

    hg qqueue --create foo // Create a new queue called "foo" and make it active
    hg qqueue --create bar // Create a new queue called "bar" and make it active
    hg qqueue foo          // Switch back to queue "foo"

## Commands
 - **qnew**: create a new patch
 - **qpop**: pop the current patch off the stack
 - **qpush**: push the next patch onto the stack
 - **qrefresh**: update the current patch
 - **qapplied**: print the patches already applied
 - **qseries**: print the entire series file
 - **qfinish**: move applied patches into repository history
 - **qdelete**: remove patches from queue
 - **qdiff**: diff of the current patch and subsequent modifications
 - **qclone**: clone main and patch repository at same time
 - **qfold**: fold the named patches into the current patch
 - **qgoto**: push or pop patches until named patch is at top of stack
 - **qguard**: set or print guards for a patch
 - **qheader**: print the header of the topmost or specified patch
 - **qimport**: import a patch or existing changeset
 - **qnext**: print the name of the next pushable patch
 - **qprev**: print the name of the preceding applied patch
 - **qqueue**: manage multiple patch queues
 - **qrecord**: interactively record a new patch
 - **qrename**: rename a patch
 - **qselect**: set or print quarded patches to push
 - **qtop**: print the name of the current patch
 - **qunapplied**: print the patches not yet applied

Deprecated:

 - **qinit**: init a new queue repository (DEPRECATED)
 - **qcommit**: commit changes in the queue repository (DEPRECATED)
 - **qsave**: save current queue state (DEPRECATED)
 - **qsave**: restore the queue state saved by a revision (DEPRECATED)

