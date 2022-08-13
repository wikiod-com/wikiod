---
title: "Cloning a repository from GitHub"
slug: "cloning-a-repository-from-github"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
 - git clone github.com/username/repository

## Clone a repository
 1. Go to the repository you want to clone (something like: https://github.com/*username*/*repo*)

[![Gif showing example of going onto the repository you want to clone.][1]][1]
 
2. On the right, click on the green button named *clone or download*

[![Gif showing example of clicking the green button named "Clone or Download"][2]][2]
 
3. A small window will appear, copy the url (something like: https://github.com/*username*/*repo*.git)

[![Gif showing example of copying link to clipboard][3]][3]
 
4. Open a terminal window on the machine you want to clone that project to
 5. Navigate from the command line to the location you want to clone the project to
 6. Enter the command: git clone <copied_url_from_step_3>
 7. Press Enter
 
8. Something like the following will appear:

    Cloning into `<repo_name>`...

    remote: Counting objects: 10, done.

    remote: Compressing objects: 100% (8/8), done.

    remove: Total 10 (delta 1), reused 10 (delta 1)    

    Unpacking objects: 100% (10/10), done.


  [1]: https://i.stack.imgur.com/E6TiT.gif
  [2]: https://i.stack.imgur.com/tQyM8.gif
  [3]: https://i.stack.imgur.com/lQFbj.gif

