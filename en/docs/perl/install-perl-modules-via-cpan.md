---
title: "Install Perl modules via CPAN"
slug: "install-perl-modules-via-cpan"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Installing modules manually
If you don't have permissions to install perl modules, you may still install them manually, indicating a custom path where you've got writing permissions.

Fist, download and unzip module archive:

    wget module.tar.gz
    tar -xzf module.tar.gz
    cd module

Then, if the module distribution contains a `Makefile.PL` file, run:

    perl Makefile.PL INSTALL_BASE=$HOME/perl
    make
    make test
    make install

or if you have `Build.PL` file instead of a `Makefile.PL`:

    perl Build.PL --install_base $HOME/perl
    perl Build
    perl Build test
    perl Build install

You also have to include the module path in `PERL5LIB` environment variable in order to use it in your code:

    export PERL5LIB=$HOME/perl

## cpanminus, the lightweight configuration-free replacement for cpan
**Usage**
 
To install a module (assuming `cpanm` is already installed):

    cpanm Data::Section

`cpanm` ("cpanminus") strives to be less verbose than `cpan` but still captures all of the installation information in a log file in case it is needed. It also handles many "interactive questions" for you, whereas `cpan` doesn't. 

`cpanm` is also popular for installing dependencies of a project from, e.g., GitHub.  Typical use is to first `cd` into the project's root, then run

    cpanm --installdeps .

With `--installdeps` it will:

 1. Scan and install *configure_requires* dependencies from either
     - META.json
     - META.yml (if META.json is missing)
 2. Build the project (equivalent to `perl Build.PL`), generating MYMETA files
 3. Scan and install *requires* dependencies from either
     - MYMETA.json
     - MYMETA.yml (if MYMETA.json is missing)

 To specify the file 'some.cpanfile', containing the dependencies, run:
    
    cpanm --installdeps --cpanfile some.cpanfile .
    

---

**`cpanm` Installation**

There are [several ways to install it][1]. Here's installation via `cpan`:

    cpan App::cpanminus

  [1]: https://metacpan.org/pod/App::cpanminus#INSTALLATION

---

**`cpanm` Configuration**

There is **no** config file for `cpanm`.  Rather, it relies on the following environment variables for its configuration:

 - `PERL_CPANM_OPT` (General cpanm command line options)
     - `export PERL_CPANM_OPT="--prompt"` # in .bashrc, to enable prompting, e.g.
     - `setenv PERL_CPANM_OPT "--prompt"` # in .tcshrc
 - `PERL_MM_OPT` (ExtUtils::MakeMaker command line options, affects module install target)
 - `PERL_MB_OPT` (Module::Build command line options, affects module install target)

## Run Perl CPAN in your terminal (Mac and Linux) or command prompt (Windows)
Command line
============

You can use `cpan` to install modules directly from the command line:

    cpan install DBI

This would be followed by possibly many pages of output describing exactly what it is doing to install the module. Depending on the modules being installed, it may pause and ask you questions.

Interactive Shell
=================

You can also enter a "shell" thus:

    perl -MCPAN -e "shell"

It will produce output as below:

    Terminal does not support AddHistory.

    cpan shell -- CPAN exploration and modules installation (v2.00)
    Enter 'h' for help.

    cpan[1]>

Then you can install the modules which you want by the easy command `install <module>`.

Example: `cpan[1]>` `install DBI`

After installing successfully, type `exit` to quit.

