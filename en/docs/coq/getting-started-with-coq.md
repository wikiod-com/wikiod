---
title: "Getting started with coq"
slug: "getting-started-with-coq"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## A simple proof
    Theorem my_first_theorem : 1 + 1 = 2.
    Proof.
      reflexivity.
    Qed.

[Try it in your browser](https://x80.org/collacoq/orosayofop.coq).

## Installation with Nix
**Warning:** this is not the standard way of installing Coq.

For users of Linux (and MacOS) who wish to gain access to up-to-date versions of Coq or to be able to use several versions of Coq on the same machine, without the hassle of using opam, and without having to compile from source, this is an alternative solution.

[Nix](http://nixos.org/nix/) is a package manager for Unix-type OS such as Linux and MacOS. It comes with its own collection of packages which is generally kept much more up-to-date than Debian's or Ubuntu's.
It does not conflict with your distribution's package manager because it does not install anything in `/usr/bin` and such.

First, you need to [install Nix](http://nixos.org/nix/manual/#ch-installing-binary):

    $ curl https://nixos.org/nix/install | sh

To ensure that the necessary environment variables are set, either log in again, or type:

    . $HOME/.nix-profile/etc/profile.d/nix.sh

Then the following command will install the latest version of Coq:

    $ nix-env -iA nixpkgs.coq_8_6

You can also run CoqIDE without adding anything to your PATH:

    $ nix-shell -p coq_8_6 --run coqide

Similarly (supposing you already have Emacs and Proof-General installed):

    $ nix-shell -p coq_8_6 --run emacs

This is very useful to run different versions when you need them. For instance, to run Coq 8.5 use the following command:

    $ nix-shell -p coq_8_5 --run coqide


## Install Coq on MacOS
You can install the whole bundle by downloading the dmg package from [here](https://coq.inria.fr/download). 

The bundle contains a CoqIDE that can be used for writing your proofs or you can use `coqtop` command to run the interpreter on your terminal

Installation of Coq on MacOS is easy using homebrew as well

`brew install coq`

or if you use MacPorts

`sudo port install coq`

There is no good vi support for Coq. You can use Proof General within emacs which has a good usability.

To install Proof General remove old versions of Proof General clone the new version from GitHub

<!-- language: bash -->
    git clone https://github.com/ProofGeneral/PG ~/.emacs.d/lisp/PG
    cd ~/.emacs.d/lisp/PG
    make

Then add the following to your .emacs:

<!-- language: lisp -->
    ;; Open .v files with Proof General's Coq mode
    (load "~/.emacs.d/lisp/PG/generic/proof-site")

Make sure that the emacs you are running the actual Emacs. If you face version mismatch problems you might have to run makefile again specifying Emacs path explicitly

<!-- language: bash -->
    make clean; make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

## Testing and installating Coq
## Testing without installing

For new users who wish to start testing Coq without installing it on their machine, there is [an online IDE called JsCoq](https://x80.org/collacoq/) (presentation [here](https://x80.org/rhino-coq/)). The package sub-window allows testing various well-known additional packages.

## Installation

The [download page](https://coq.inria.fr/download) contains installers for Windows and MacOS.

Users of Linux are generally advised to compile from source using opam, in order to get the latest version. Basic instructions on how to do so are given [here](https://coq.inria.fr/opam/www/using.html).


## Example of a proof by induction
Here is a simple proof by induction.

    Require Import Coq.Setoids.Setoid.
    Require Import Coq.Arith.Lt.

    (* A number is less than or equal to itself *)
    Theorem aLTEa : forall a,
        a <= a.
        auto with arith. (* This follows by simple arithmetic *)
        Qed.

    Theorem simplALTE : forall a b,
        S a <= S b <-> a <= b. (* If a <= b, then a + 1 <= b + 1 *)
    Proof.
    Admitted.

    Theorem ltAlwaysLt: forall a b,
        a <= a + b.
    Proof.
      intros. (* Introduce relevant variables *)
      induction a, b. (* Induction on every variable *)
      simpl. apply aLTEa. (* 0 <= 0 + S b *)
      rewrite -> plus_O_n. auto with arith. (* 0 <= S b *)
      rewrite <- plus_n_O. apply aLTEa. (* S a <= S a + 0 *)
      rewrite <- simplALTE in IHa. (* IHa: a <= a + S b. Goal: S a <= S a + S b. *)
      apply IHa. (* We rewrote the induction hypothesis to be in the same form as the goal, so it applies immediately now *)
      Qed.

