---
title: "Update a forked Repository"
slug: "update-a-forked-repository"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

- [GitHub Help: Configuring a remote for a fork](https://help.github.com/articles/configuring-a-remote-for-a-fork/)
- [GitHub Help: Syncing a fork](https://help.github.com/articles/syncing-a-fork/)
- [popular ans in StackOverFlow](http://stackoverflow.com/questions/7244321/how-do-i-update-a-github-forked-repository)

## Config a remote for your fork then sync your fork (master branch)
1. Config a remote for my fork
```
$ cd my_local_repo

$ git remote add upstream https://github.com/ORIGINAL_OWNER/ORIGINAL_REPOSITORY.git
    # Specify a new remote upstream repository that will be synced with the fork

$ git remote -v
    # Verify the new upstream repository specified for my fork
```

2. Sync my fork locally
```
$ cd my_local_repo

$ git fetch upstream
    # Fetch the branches and their respective commits from the upstream repository
    # Commits to master will be stored in a local branch, upstream/master

$ git checkout master

$ git merge upstream/master
    # Merge the changes from upstream/master into your local master branch
    # brings your fork's master branch into sync with the upstream repo
```
3. Sync my fork on Github
```
$ git push origin master
```

