---
title: "aws-codecommit for local git"
slug: "aws-codecommit-for-local-git"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Prepare by setting up your local development machine with the [aws command line tool][1] and the [git][2] command.


  [1]: https://www.wikiod.com/aws-cli/getting-started-with-aws-cli
  [2]: https://www.wikiod.com/git

## Setup Codecommit for git command line
AWS Codecommit can be used as storage for private GIT repositories. The setup involves a few steps, assuming you have a valid AWS account already.

1. Sign up for [AWS Codecommit][1]. Currently only region `us-east-1` is available.
2. Create a [IAM user][2] who will have access to the repositories, eg `codecommit-user`
3. Attach permission role `AWSCodeCommitFullAccess` to this user
4. Create a new `Access Key` for this user and note `key id` and `secret code`
5. Now go ahead and create a [AWS Configuration profile][3] on your local machine


    $ aws configure --profile codecommit-user

In the next step we associate the `aws` command with `git` as the credential helper with the following commands:

    $ git config --global credential.helper \
        '!aws --profile codecommit-user codecommit credential-helper $@'
    $ git config --global credential.UseHttpPath true

You can verify or edit this setup afterwards:

    $ git config --global --edit

You should note a section:

    [credential]
        helper = !aws --profile codecommit-user codecommit credential-helper $@
        UseHttpPath = true

Now you can use git from the command line as usual.

  [1]: https://console.aws.amazon.com/codecommit/home?region=us-east-1#/repository/list
  [2]: https://console.aws.amazon.com/iam/home#users
  [3]: https://www.wikiod.com/aws-cli/getting-started-with-aws-cli#Creating a New Profile

## Use SourceTree with AWS Codecommit
Atlassian [SourceTree][1] is a visual tool for Mac and Windows to manage source code repositories. This can be used with Codecommit as a remote repository but need to add an extra configuration option to the local repository in SourceTree to be able to connect with codecommit.

First, setup Codecommit for local git.

Assuming you have a local `git` repository which you want to push to `codecommit` just follow these steps:

1. Login to AWS Codecommit using the [web console][2].
2. Create a new repository, eg `my-project`
3. Copy the HTTPS URL, it should look like `https://git-codecommit.us-east-1.amazonaws.com/v1/repos/my-project`
4. Now in SourceTree open the panel Settings / Remotes
5. Add new remote with name: `origin` and Url / Path: the link you copied before
6. Finally open the option Edit Config File and add the following snippet: 


    [credential]
        helper = /usr/local/bin/aws --profile codecommit-user codecommit credential-helper $@
        UseHttpPath = true

[![enter image description here][3]][3]


After saving the config file should look something like this:


    [core]
        repositoryformatversion = 0
        filemode = true
        bare = false
        logallrefupdates = true
        ignorecase = true
        precomposeunicode = true
    [branch "master"]
        remote = origin
        merge = refs/heads/master
    [remote "origin"]
        url = https://git-codecommit.us-east-1.amazonaws.com/v1/repos/digitaloffice.nu
        fetch = +refs/heads/*:refs/remotes/origin/*
    [credential]
        helper = /usr/local/bin/aws --profile codecommit-user codecommit credential-helper $@
        UseHttpPath = true
 

Please note: this is based on OS-X setup. Take special care of the path for aws (which is `/usr/local/bin/aws` in this case) and will most certainly be different under other Unixes or Windows configurations.


  [1]: https://www.atlassian.com/software/sourcetree
  [2]: https://console.aws.amazon.com/codecommit/home?region=us-east-1#/repository/list
  [3]: http://i.stack.imgur.com/s3UeG.png

