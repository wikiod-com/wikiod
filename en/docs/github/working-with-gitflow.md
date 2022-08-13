---
title: "Working with Gitflow"
slug: "working-with-gitflow"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
 - git flow \<subcommand\>
 - git flow init
 - git flow [feature|release|hotfix] [start|finish]

## Parameters
| Subcommand | Details |
|-----|-----|
| init      | Initialize a new git repo with support for the branching model. |
| feature   | Manage your feature branches. |
| release   | Manage your release branches. |
| hotfix    | Manage your hotfix branches. |


- [gitflow concept from author](http://nvie.com/posts/a-successful-git-branching-model/)
- [branch model picture](http://nvie.com/img/git-model@2x.png)

## Operation on 5 common branches locally
One of most common use cases of Gitflow

1. _Initialize_ repo and define branches

```
$ git flow init
    # if you use default setup, you'll define six types of branches:
    #
    # main branches (lives forever)
    #
    #   1. master:  for production releases
    #   2. develop: for "next release" development
    #
    # supporting branches
    #
    #   3. feature: for a product feature
    #   4. release: for preparation of a new production release
    #   5. hotfix:  for resolving critical bug of production version
    #   6. support
    #
    # also, two main branches are created: master, develop
```

2. _Start_ and _Finish_ a Feature

```
$ git flow feature start my_feature
    # create branch 'feature/my_feature' based on the 'develop'

    # made development and commits...

$ git flow feature finish my_feature
    # merge 'feature/my_feature' back to the 'develop'
    # delete 'feature/my_feature'
```

3. _Start_ and _Finish_ a Release

```
$ git flow release start my_release
    # create branch 'release/my_release' based on the 'develop'

    # made bug fixes...

$ git flow release finish my_release
    # merge branch 'release/my_release' to the 'master' and add tag
    # merge branch 'release/my_release' back to the 'develop'
    # delete 'release/my_release'
```

4. _Start_ and _Finish_ a Hotfix

```
$ git flow hotfix start my_hotfix
    # create branch 'hotfix/my_hotfix' based on the 'master'

    # made some hotfixes...

$ git flow hotfix finish my_hotfix
    # merge branch 'hotfix/my_hotfix' back to the 'master' and add tag
    # merge branch 'hotfix/my_hotfix' to the 'develop'
    # delete 'hotfix/my_hotfix'
```



