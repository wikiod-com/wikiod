---
title: "Publishing"
slug: "publishing"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Publish public repository
To publish public repositories with a free npm account, you initially need to publish the module with access of public. One way to do this is to set the config for `npm` to read in `packages.json` as follows:

    "publishConfig": {
      "access": "public"
    },

you can also use the flag `--access=public` with the `npm publish` command. Otherwise you will get the slightly confusing error message, "you need a paid account to perform this action".

    npm publish // if you modified packages.json
or

    npm publish --access=public


## Sign Git Tag
For signing to work you need a default GPG key configured. You can turn it on or off as follows:

    npm config set sign-git-tag <true or false>

