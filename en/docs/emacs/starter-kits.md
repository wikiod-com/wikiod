---
title: "Starter Kits"
slug: "starter-kits"
draft: false
images: []
weight: 9922
type: docs
toc: true
---

Starter kits enable _new users_ to start using Emacs quickly and avoid
some of the setup hurdles that come from a mature system like Emacs --
one that has grown through decades of evolution and naturally has some
historical quirks.  _Experienced users_ also benefit from having a kit
configuration of extensions that are curated by others.

It requires considerable effort to maintain a set of packages and
settings that will continue to work well together as packages improve
(or bit-rot) over time.  Many Emacs users don't desire to do this
maintenance, so they turn to starter kits.  Assembly and maintenance
of a kit bears a small-scale resemblance to management of a Linux
distribution.


## Themes and Customization

Some starter kits are themed; e.g., for specific programming language
environments, or music creation, or emulation of another editor.
Others aim to provide a kitchen sink of bundling
comfortable/productive modules for as many situations or languages as possible.

Most starter kits have provisions for extension and customization.
A user will override particular key bindings and settings, and be able to add
packages that are not yet provided.

## Popular Kits

There are many starter kits available.  In theory, anyone who
publishes their `~/.emacs.d` has created one.  But a handful have
become popular and well maintained by one or more individuals.  Some
examples (in order of subjective popularity based on Github stars) include
[Spacemacs](https://github.com/syl20bnr/spacemacs),
[Prelude](http://batsov.com/prelude/),
[Purcell](https://github.com/purcell/emacs.d),
[Emacs Starter Kit](https://github.com/technomancy/emacs-starter-kit),
[Magnars](https://github.com/magnars/.emacs.d), and
[Emacs Live](http://overtone.github.io/emacs-live/).  More details are
listed in the **Examples** section above and more starter kits are listed [on this wiki](http://wikemacs.org/wiki/Starter_Kits).

A notable "micro-kit" is
[Sane Defaults](https://github.com/magnars/.emacs.d/blob/master/settings/sane-defaults.el),
providing a handful of settings to remove some of Emacs' default
surprise-to-newcomers behaviors.

## Is a starter kit needed?

Although there is
[some controversy](https://www.reddit.com/r/emacs/comments/1udtd1/starting_emacs_with_preludestarter_kits_scares_me/)
around using starter kits, for many the benefits can far outweigh the
cost figuring out how to harmonize a dynamic Emacs setup.  Arguments
against starter kits usually pertain to: users being unaware of some
of the nuances and native behavior of Emacs, being difficult to debug,
and even making Emacs look more like a foreign editor (Spacemacs).


## Spacemacs
[Spacemacs][1] is a popular starter kit for emacs. It features a robust package management solution and centers around emacs's popular [evil mode][2], which provides many of the keybindings from [vim][3].

It is called Spacemacs because it uses the <kbd>Space</kbd> key as the leader key (the idea is similar to Vim's leader key).

Installation is pretty easy. Just download and install the standard emacs distribution and then clone the git repo:

    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

Or, you can download a zip locally from the website and just copy it to `~/.emacs.d`

Once you have it downloaded (cloned), launch it, then press the space bar to explore the interactive list of carefully-chosen key bindings. You can also press the home buffer's [?] button for some first key bindings to try.


  [1]: http://spacemacs.org/
  [2]: https://www.emacswiki.org/emacs/Evil
  [3]: http://www.vim.org/

## Prelude
[Prelude][1] is another popular starter kit. It features good support for various programming languages out-of-the-box including, notably - clojure. On *nix systems it can be installed with the following command:

    curl -L https://git.io/epre | sh



  [1]: https://github.com/bbatsov/prelude

## emacs-live
[emacs-live][1] is another popular emacs starter kit, with an additional focus on live music coding using [overtone][2].

You can install it in 2 ways:

 1. On *nix (e.g. linux, OSX, etc.) systems, run the following command on the command-line:

    

> bash <(curl -fksSL
> https://raw.github.com/overtone/emacs-live/master/installer/install-emacs-live.sh)

 2. - Download the zip from the github page.
    - Backup your current `~/.emacs.d` in your home directory
    - extract the zip you downloaded and move it to `~/.emacs.d`:


  [1]: https://github.com/overtone/emacs-live
  [2]: http://overtone.github.io/



## Scimax
[Scimax][1] is an Emacs starter kit focused on reproducible research, targeted mainly at scientists and engineers. Scimax customizes Org-Mode with features that make cross-referencing, exporting, and coding (in particular Python), simpler.

Installation instructions can be found [on the landing page of the project][2].


  [1]: https://github.com/jkitchin/scimax/blob/master/scimax.org
  [2]: https://github.com/jkitchin/scimax

