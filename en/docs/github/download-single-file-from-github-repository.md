---
title: "download single file from GitHub repository"
slug: "download-single-file-from-github-repository"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## from a public repository using the command line and renaming file
This example grabs the Node.gitignore file from GitHub's gitignore repository, downloads it to your current working directory and renames it to .gitignore - all very typical actions for someone starting a new node.js project.

    $ curl http://github.com/github/gitignore/raw/master/Node.gitignore -o .gitignore


## find the url of the file you want to download
1. navigate to the desired file in a repository
2. click the 'raw' button
3. copy the url from the address bar

See the following example from GitHub's gitignore repository: http://github.com/github/gitignore/raw/master/Node.gitignore

You can quickly recognize a url that will work to download an individual file vs downloading the html page. Look for the subdirectory /raw/ right before the branch name.

