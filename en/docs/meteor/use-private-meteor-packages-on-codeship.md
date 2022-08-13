---
title: "Use Private Meteor Packages on Codeship"
slug: "use-private-meteor-packages-on-codeship"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Note that we did not discuss how to use & develop your local packages. There are several ways, I suggest to use the `PACKAGE_DIRS` environment variable described by [David Weldon on his website](https://dweldon.silvrback.com/local-packages).

## Install MGP
We make use of Dispatches great [Meteor Github Packages (mgp)](https://github.com/DispatchMe/mgp) package:

    npm install --save mgp


Then, add the following command to your ```package.json``` scripts:

    "mgp": "mgp"

Create a file named ```git-packages.json``` in your project root. Add a config for every (private) Meteor Github package that your project depends on:

    {
      "my:yet-another-private-package": {
        "git": "git@github.com:my/private-packages.git",
        "branch": "dev"
      }
    }

More information about how to configure your private packages can be found on the [projects Github repo.](https://github.com/DispatchMe/mgp)

## Configure Codeship to Install Private Github Packages
Append the following command to the Codeship setup commands:

    meteor npm run mgp

Now, we need to give Codeship access to these private repositories. There is a [Codeship documentation article](https://documentation.codeship.com/faq/access-to-other-repositories-fails-during-build/) describing this process in detail but here are the steps that you have to take for Github:

- Create a new Github account. A so called [Machine user](https://developer.github.com/guides/managing-deploy-keys/#machine-users).
- Remove the deploy key from your repo under test. Here: https://github.com/YOUR_USERNAME/REPO_UNDER_TEST/settings/keys
- Grab the SSH public key from your codeship projects settings. Somewhere here: https://codeship.com/projects/PROJECT_NUMBER/configure
- Add this SSH public key to your machine user's SSH keys: https://github.com/settings/keys
- Give this machine user access to all your referenced repositories

It should be similar for BitBucket and others.


