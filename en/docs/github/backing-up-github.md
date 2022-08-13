---
title: "Backing up GitHub"
slug: "backing-up-github"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Cloning all repositories for a username
Run the following command, replacing username with the username, to clone all of the GitHub repositories for that user to the current directory.

````
curl "https://api.github.com/users/username/repos?page=1&per_page=100" | grep -e 'git_url*' | cut -d \" -f 4 | xargs -L1 git clone
````

This will only clone the first 100 repositories.

