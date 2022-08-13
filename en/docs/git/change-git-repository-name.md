---
title: "Change git repository name"
slug: "change-git-repository-name"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

If you change repository name on the remote side, such as your github or bitbucket, when you push your exisiting code, you will see error: Fatal error, repository not found**. 


## Change local setting
Go to terminal,

    cd projectFolder
    git remote -v (it will show previous git url)
    git remote set-url origin https://username@bitbucket.org/username/newName.git
    git remote -v (double check, it will show new git url)
    git push (do whatever you want.)


