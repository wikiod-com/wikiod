---
title: "Using Gist"
slug: "using-gist"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Gists are a great way to share your work. You can share single files, parts of files, or full applications. You can access gists at https://gist.github.com.

Every gist is a Git repository, which means that it can be forked and cloned. The gist editor is powered by CodeMirror.

There are two types of gists: public gists and secret gists. 

Additionally, if you are not logged into GitHub when you create your gist, it will be an anonymous gist.

Gists are a great way to share your work. You can share single files, parts of files, or full applications. 

There are two types of gists: public gists and secret gists. Additionally, if you are not logged into GitHub when you create your gist, it will be an anonymous gist.


**Public Gists**

Public gists show up in Discover, where people can browse new gists as they're created. They're also searchable, so you can use them if you'd like other people to find and see your work.

**Secret Gists**

Secret gists don't show up in Discover and are not searchable. Use them to jot down an idea that came to you in a dream, create a to-do list, or prepare some code or prose that's not ready to be shared with the world.

You can create as many secret gists as you like.

**Anonymous Gists**

If you create a gist without logging into GitHub, it will be an anonymous gist. Anonymous gists can be public or secret. To delete an anonymous gist on GitHub.com or GitHub Enterprise, contact GitHub support or your site administrator, respectively. Please provide the URL of the gist you wish to delete.

## Public Gist
A public gist can be *almost* anything.

A simple example of a Javascript function:

    function randomInt(min, max) {
    return Math.floor((max - min + 1) * Math.random()) + min;
    }

## Secret Gist
A secret gist should be used for anything that you don't want to appear publicly on GitHub. Secret gists can be used when you don't want private keys to be accessible to the public, or for and private code in general.

A simple example of JSON code that would be better fit for a secret gist:

    {
    "id": AKIAIOSFODNN7EXAMPLE,
    "secret": wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
    }

