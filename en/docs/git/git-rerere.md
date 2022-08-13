---
title: "Git rerere"
slug: "git-rerere"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

`rerere` (reuse recorded resolution) allows you to tell git to remember how you resolved a hunk conflict. This allows it to be automatically resolved the next time that git encounters the same conflict.

## Enabling rerere
To enable `rerere` run the following command:

    $ git config --global rerere.enabled true
This can be done in a specific repository as well as globally.

