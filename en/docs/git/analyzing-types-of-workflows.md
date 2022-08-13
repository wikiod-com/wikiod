---
title: "Analyzing types of workflows"
slug: "analyzing-types-of-workflows"
draft: false
images: []
weight: 9851
type: docs
toc: true
---

Using version control software like Git may be a little scary at first, but its intuitive design specializing with branching helps make a number of different types of workflows possible. Pick one that is right for your own development team.

## Gitflow Workflow
Originally proposed by [Vincent Driessen][1], Gitflow is a development workflow using git and several pre-defined branches. This can seen as a special case of the [Feature Branch Workflow][2].

The idea of this one is to have separate branches reserved for specific parts in development:

* `master` branch is always the most recent *production* code. Experimental code does not belong here. 
* `develop` branch contains all of the latest *development*. These developmental changes can be pretty much anything, but larger features are reserved for their own branches. Code here is always worked on and merged into `release` before release / deployment.
* `hotfix` branches are for minor bug fixes, which cannot wait until the next release. `hotfix` branches come off of `master` and are merged back into both `master` and `develop`.
* `release` branches are used to release new development from `develop` to `master`. Any last minute changes, such as bumping version numbers, are done in the release branch, and then are merged back into `master` and `develop`. When deploying a new version, `master` should be tagged with the current version number (e.g. using [semantic versioning][3]) for future reference and easy rollback.
* `feature` branches are reserved for bigger features. These are specifically developed in designated branches and integrated with `develop` when finished. Dedicated `feature` branches help to separate development and to be able to deploy *done* features independently from each other.   

A visual representation of this model:

[![Credit to Atlassian for providing this image][4]][4]

The original representation of this model:

[![Git flow original graphical representation][5]][5]


  [1]: http://nvie.com/posts/a-successful-git-branching-model/
  [2]: https://www.wikiod.com/git/analyzing-types-of-workflows#Feature Branch Workflow
  [3]: http://semver.org/
  [4]: http://i.stack.imgur.com/TBHkD.png
  [5]: http://i.stack.imgur.com/RGIng.png

## Centralized Workflow
With this fundamental workflow model, a `master` branch contains all active development. Contributors will need to be especially sure they pull the latest changes before continuing development, for this branch will be changing rapidly. Everyone has access to this repo and can commit changes right to the master branch.

Visual representation of this model:

[![Atlassian][1]][1]


  [1]: http://i.stack.imgur.com/dAYXB.png

This is the classic version control paradigm, upon which older systems like Subversion and CVS were built. Softwares that work this way are called Centralized Version Control Systems, or CVCS's. While Git is capable of working this way, there are notable disadvantages, such as being required to precede every pull with a merge. It's very possible for a team to work this way, but the constant merge conflict resolution can end up eating a lot of valuable time.

This is why Linus Torvalds created Git not as a CVCS, but rather as a *DVCS*, or *Distributed Version Control System*, similar to Mercurial. The advantage to this new way of doing things is the flexibility demonstrated in the other examples on this page.

## Feature Branch Workflow
The core idea behind the Feature Branch Workflow is that all feature development should take place in a dedicated branch instead of the `master` branch. This encapsulation makes it easy for multiple developers to work on a particular feature without disturbing the main codebase. It also means the `master` branch will never contain broken code, which is a huge advantage for continuous integration environments.  

Encapsulating feature development also makes it possible to leverage pull requests, which are a way to initiate discussions around a branch. They give other developers the opportunity to sign off on a feature before it gets integrated into the official project. Or, if you get stuck in the middle of a feature, you can open a pull request asking for suggestions from your colleagues. The point is, pull requests make it incredibly easy for your team to comment on each other’s work.

based on [Atlassian Tutorials][1].


  [1]: https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow

## GitHub Flow
Popular within many open source projects but not only.

**Master** branch of a specific location (Github, Gitlab, Bitbucket, local server) contains the latest shippable version. For each new feature/bug fix/architectural change each developer creates a branch.

Changes happen on that branch and can be discussed in a pull request, code review, etc. Once accepted they get merged to the master branch.

Full flow by Scott Chacon:

-    Anything in the master branch is deployable
-    To work on something new, create a descriptively named branch off of master (ie: new-oauth2-scopes)
-    Commit to that branch locally and regularly push your work to the same named branch on the server
-    When you need feedback or help, or you think the branch is ready for merging, open a pull request
-    After someone else has reviewed and signed off on the feature, you can merge it into master
-    Once it is merged and pushed to ‘master’, you can and should deploy immediately

Originally presented on [Scott Chacon's personal web site](http://scottchacon.com/2011/08/31/github-flow.html).

[![Visualization of GitHub Workflow][1]][1]

Image courtesy of the [GitHub Flow reference](https://guides.github.com/introduction/flow/)


  [1]: http://i.stack.imgur.com/KoMdO.png

## Forking Workflow
This type of workflow is fundamentally different than the other ones mentioned on this topic. Instead of having one centralized repo that all developers have access to, each developer has his/her *own* repo that is forked from the main repo. The advantage of this is that developers can post to their own repos rather than a shared repo and a maintainer can integrate the changes from the forked repos into the original whenever appropriate.

A visual representation of this workflow is as follows:

[![Atlassian][1]][1]


  [1]: http://i.stack.imgur.com/FAI5q.png

