---
title: "Deployment"
slug: "deployment"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- git push heroku master

## Deploying with Git
# Tracking your app in git
Before you can push an app to Heroku, you’ll need to initialize a local Git repository and commit your files to it. For example, if you have an app in a directory, myapp, then create a new repository for it:

    $ cd myapp
    $ git init
    Initialized empty Git repository in .git/
    $ git add .
    $ git commit -m "my first commit"
    Created initial commit 5df2d09: my first commit
     44 files changed, 8393 insertions(+), 0 deletions(-)
     create mode 100644 README
     create mode 100644 Procfile
     create mode 100644 app/controllers/source_file
    ...

This is a local repository, now residing inside the `.git` directory. Nothing has been sent anywhere yet; you’ll need to create a remote and do a push to deploy your code to Heroku.

# Creating a Heroku remote

    $ heroku create
    Creating falling-wind-1624... done, stack is cedar-14
    http://falling-wind-1624.herokuapp.com/ | https://git.heroku.com/falling-wind-1624.git
    Git remote heroku added

Git repository with an existing application. The heroku git:remote command will add this remote for you based on your applications git url.

    $ heroku git:remote -a falling-wind-1624
    Git remote heroku added.

# Deploying code
you'll need to specify a remote branch to push to. You can do your first push:

    $ git push heroku master
    Initializing repository, done.
    updating 'refs/heads/master'
    ...
To push a branch other than master, use this syntax:

    $ git push heroku yourbranch:master



