---
title: "Getting started with valgrind"
slug: "getting-started-with-valgrind"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
[Valgrind](http://valgrind.org/) is GPLv2-licensed collection of dynamic analysis tools, which uses binary instrumentation (dynamic recompilation). Six tools are included to detect memory management (Memcheck) and threading errors (Helgrind and DRD), to generate call-graph and profile programs (with optional cache and branch-prediction simulation - Cachegrind and Callgrind), to profile heap memory usage (Massif).

Valgrind supports several platforms: X86/Linux, AMD64/Linux, ARM/Linux, ARM64/Linux, PPC32/Linux, PPC64/Linux, PPC64LE/Linux, S390X/Linux, MIPS32/Linux, MIPS64/Linux, X86/Solaris, AMD64/Solaris, ARM/Android (2.3.x and later), ARM64/Android, X86/Android (4.0 and later), MIPS32/Android, X86/Darwin and AMD64/Darwin (Mac OS X 10.12).

Valgrind is included into many Linux distributions: Debian & [Ubuntu](https://wiki.ubuntu.com/Valgrind) (`sudo apt-get install valgrind`), Arch (`sudo pacman -S valgrind), Fedora (`sudo yum -y install valgrind`, `sudo dnf install valgrind`). It can also [be built from source](http://valgrind.org/docs/manual/dist.install.html).

