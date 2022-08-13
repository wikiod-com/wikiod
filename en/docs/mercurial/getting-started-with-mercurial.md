---
title: "Getting started with mercurial"
slug: "getting-started-with-mercurial"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
You can [download Mercurial][1] from the project's website, and there are [graphical utilities][2] for Windows, Linux and OSX if you'd prefer that to a command line interface. Most Unix package managers include Mercurial, for example on Debian/Ubuntu:

    $ apt-get install mercurial

You can verify Mercurial is installed by running:

    $ hg --version

## Setup

Mercurial works out of the box, but you'll likely want to configure Mercurial to know who you are before you go further. To associate a username with your commits edit `~/.hgrc` (or `mercurial.ini` in your home directory on Windows) and add the following lines:

    [ui]
    username = Your Name <your@email.address>

If you don't want to do this you can always specify a username when you commit with the `-u` flag, e.g.:

    $ hg commit -u "Your Name <your@email.address>"


  [1]: https://www.mercurial-scm.org/downloads
  [2]: http://tortoisehg.bitbucket.org/

## Getting Started
*See also the [Mercurial Tutorial][1]*

## Creating a Mercurial Repository

A Mercurial repository is simply a directory (referred to as the "working directory") containing an `.hg` directory with metadata about the contents of the repository. This makes Mercurial very lightweight and easy to start using. To create a new repository simply run:

    $ hg init project

Where `project` is the name of the directory you'd like to create. This creates a `project` directory along with a `project/.hg` directory containing the repository itself.

       $ cd project
       $ echo Hello World > hello.txt
       $ hg stat
       ? hello.txt

We just created a `hello.txt` file in the repository and ran [`hg status`](https://selenic.com/hg/help/status) (or `stat` for short) to see the current status of our repository. As you can see `hello.txt` is annotated with a `?`, meaning Mercurial isn't yet aware of it. The [`add`](https://selenic.com/hg/help/add) command registers this new file with Mercurial so it will be included in the next commit.

    $ hg add hello.txt

Now that Mercurial is aware of a changed file you can run `diff` to see exactly what's changed since the last commit - in this case we're adding the full contents of `hello.txt`:

    $ hg diff
    diff -r 000000000000 hello.txt
    --- /dev/null   Thu Jan 01 00:00:00 1970 +0000
    +++ b/hello.txt Sat Jul 23 01:38:44 2016 -0400
    @@ -0,0 +1,1 @@
    +Hello

And once we're happy with them and ready to check in our changes we can run [`commit`](https://selenic.com/hg/help/commit):

    $ hg commit -m "Created a hello world file."

Note that we included a commit message with `-m` - if you don't specify `-m` Mercurial will launch a text editor you can enter a commit message in. This is useful if you'd like to provide a longer multi-line message.

Once you've committed your changes they no longer show up if you run `hg stat` since the repository is now in sync with the contents of the working directory. You can run [`log`](https://selenic.com/hg/help/log) to see a list of commits, and `-v` includes additional details like the files each commit touched:

    $ hg log -v
    changeset:   0:b4c06cc77a42
    tag:         tip
    user:        Michael Diamond@Aodh <dimo414@gmail.com>
    date:        Sat Jul 23 01:44:23 2016 -0400
    files:       hello.txt
    description:
    Created a hello world file.


  [1]: https://www.mercurial-scm.org/wiki/Tutorial


## Pushing and Pulling
Mercurial makes it easy to share your work, and to pull in contributions from other developers. This involves three key steps; [cloning][1], [pulling][2], and [pushing][3].

## Clone

To copy a remote repository to your local disk you "clone" it. To do so simply pass the remote URL you'd like to clone from. To clone the Mercurial source code simply run:

    $ hg clone https://selenic.com/hg

This creates a local `hg` directory containing a copy of the Mercurial repository you can build, edit, and commit to (though you can't publish your commits back to the parent repository).

## Pull

Once you have a repository checked out, you'll want to keep it in sync as others publish changes to it. You can pull down new changes by simply running:

    $ hg pull

This pulls in new commits but doesn't update your working directory, so you won't see any changes immediately. To [update][4] the contents of the working directory run:

    $ hg up

Which updates your working directory to the tip (most recent) revision in the repository.

You can also run:

    $ hg pull -u

To pull in new changes and update the working directory in one step.

## Push

Assuming you have write-access to the remote repository you can publish any commits you've made locally to the remote repository just as easily with:

    $ hg push

This uploads your changes as long as there haven't been any other commits since the last time you pulled. If your `push` is rejected because it would "create additional heads" that means you need to pull in those new changes and merge them with your own.

    $ hg pull
    $ hg merge  # this creates a new changeset merging your changes with the remote changes
    $ hg commit -m "Merged in remote changes"
    $ hg push

Most of the time this is all you'll have to do since Mercurial handles merging your changes automatically, however sometimes you'll need to resolve merge conflicts manually (see merging topic). If you need to you can always cancel a merge and get back to a clean working directory with:

    $ hg up -c

But remember this is a destructive operation; any changes in the working directory will be erased.

  [1]: https://selenic.com/hg/help/clone
  [2]: https://selenic.com/hg/help/pull
  [3]: https://selenic.com/hg/help/push
  [4]: https://selenic.com/hg/help/update

## Branching

When we’re first starting our work, we have to decide if this is a separate area of work we’re working on, or is this part of an existing line of work.  If it’s existing, we can work off of that branch.  If it’s new, we’ll start a new branch.  

Our workflow then is:

 - `hg branch MyNewFeature`
 - work work work
 - `hg commit -m "committing my changes"`
 - work work work
 - `hg commit -m "more changes"`

At this point, we want to push our work up to the remote server. But before pushing the changes (ignore this if it is a new branch you haven't pushed before), we need to check if there are any incoming change to this branch. We can check this with:

    hg incoming -b .

If there are any incoming changesets on our branch, we need to do a pull and rebase our changes to the top of the list of changes.

    hg pull -b . --rebase

Once this is done or if there are no incoming changesets, we can proceed with the Push.

We only ever want to push our current work, not everything we’ve ever done. I really never push my entire repository, but my current line of work. The reasoning is that pushing the entire repository assumes I’m integrating multiple lines of work.  But I only want to integrate my current line of work, and I only want to work in one line at a time.

If this is the first time I’m pushing this branch:

    hg push -b . --new-branch

If I've already pushed this branch:

    hg push -b .

The “-b .” command means just push the current branch, and not anything else.

**To change between the working branches**:

    hg update myBranchName



