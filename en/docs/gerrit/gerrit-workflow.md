---
title: "Gerrit workflow"
slug: "gerrit-workflow"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Merging straight to master
If you want to make a change that you want to merge with master, the best way is to first create a topic branch

    git checkout -b foo

make a single commit with your feature

    git commit -m "Made the thing X finally work"

and push that branch to review via

    git push origin foo:refs/for/master/foo

Avoid working directly on master, as you will have problems resolving conflicts if someone else pushed to master before you. If you have your work on a separate branch, then resolving conflicts is as simple as

    git checkout master
    git pull origin master
    git checkout foo
    git rebase master
    git push origin foo:refs/for/master/foo

## Use rebase instead of merge
Because of how gerrit relies on change-ids, in order to resolve conflicts (pull changes to your topic branch) the best practice is to rebase topic branch onto master/other branch you want to push to. This way you preserve the change-id without having to ammend the merge commit. For example if you submit foo to `refs/for/master` with history as follows:
           
          a < foo
         /
     ---s < master

Then gerrit will create a change with change-id taken from the commit `a`. But say after you passed review but before you submitted to master someone submitted their change:

          a < foo
         /
     ---s---x < master

If you rebase `foo` onto `master` you'll have

              a < foo
             /
     ---s---x < master
   
And you can easily push `a` again with the same change-id.

## Working on a feature branch
As mentioned in the other example, you should use rebase instead of merge. But if you're working on a feature branch with your team then you'll run into the problem of pulling rewritten history. So the best way to work on a feature branch `foo` is to locally create tracking branch `foo` that you use only for pulling others' changes and create one more branch e.g. `dev_foo` that you use only for pushing. Then use the basic workflow as if `foo` was `master` and `dev_foo` your topic branch.

For example your history might look like this before submitting a change to review

               x < dev_foo
              /
         a---b < foo
        /
    ---s---t---u < master

To push commit `x` to review and have it submitted to `foo` use

    git push origin dev_foo:refs/for/foo/dev_foo

