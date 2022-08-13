---
title: "Installation of Perl"
slug: "installation-of-perl"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

I'm going to begin this with the process in Ubuntu, then in OS X and finally in Windows. I haven't tested it on all perl versions, but it should be a similar process.

Use [Perlbrew](https://perlbrew.pl) if you like to switch easily beween different versions of Perl.


I want to state that this tutorial is about Perl in it's open-source version. There are other versions like `activeperl` which its advantages and disadvantages, that are not part of this tutorial.
 

## Linux
There is more than one way to do it:

 - Using the package manager: 

       sudo apt install perl

 - Installing from source:
 
       wget http://www.cpan.org/src/5.0/perl-version.tar.gz
       tar -xzf perl-version.tar.gz
       cd perl-version
       ./Configure -de
       make
       make test
       make install

 - Installing in your $home directory (not sudo needed) with [Perlbrew](https://perlbrew.pl):

       wget -O - https://install.perlbrew.pl | bash

    See also [Perlbrew](https://www.wikiod.com/perl/perlbrew)


## OS X
There are several options:

- [Perlbrew][1]:

      # You need to install Command Line Tools for Xcode  
      curl -L https://install.perlbrew.pl | bash

- [Perlbrew][1] with thread support:

      # You need to install Command Line Tools for Xcode  
      curl -L https://install.perlbrew.pl | bash

    After the install of perlbrew, if you want to install Perl with thread support, just run:
     
      perlbrew install -v perl-5.26.0 -Dusethreads

- From source:

      tar -xzf perl-version.tar.gz
      cd perl-version
      ./Configure -de
      make
      make test
      make install


  [1]: https://perlbrew.pl/

## Windows
* As we said before, we go with the open-source version. For Windows you can choose `strawberry` or `DWIM`. Here we cover the `strawberry` version, since `DWIM` is based on it. The easy way here is installing from the [official executable][1]. 

See also [berrybrew][2] - the perlbrew for Windows Strawberry Perl

  [1]: http://strawberryperl.com/
  [2]: https://github.com/dnmfarrell/berrybrew

