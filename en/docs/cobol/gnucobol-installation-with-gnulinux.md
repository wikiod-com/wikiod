---
title: "GnuCOBOL installation with GNULinux"
slug: "gnucobol-installation-with-gnulinux"
draft: false
images: []
weight: 9918
type: docs
toc: true
---

## GNU/Linux install
For most GNU/Linux distributions, a version of `GnuCOBOL` is available in the repositories.  `GnuCOBOL` was originally `OpenCOBOL`, rebranded when the project became an official GNU project.  Many repositories are still using `open-cobol` as the package name (as of August 2016).

For Fedora, and other RPM based package managers

    sudo yum install open-cobol

For Debian, Ubuntu and APT based packages

    sudo apt install open-cobol

This is usually version 1.1 of the compiler suite, and will deal with the compile time and runtime dependencies required when using GnuCOBOL.

From source, (hosted on SourceForge at https://sourceforge.net/projects/open-cobol/) you will need.

- A C compiler suite; `build-essential` (or similar)
- BerkeleyDB and BerkelyDB development headers; `libdb`, `libdb-dev` (or similar names)
- GNU Multi-Precision numeric library; `libgmp`, `libgmp-dev`
- A version of `curses`; `ncurses`, `ncurses-dev`
- The source kit, `gnucobol-1.1.tar.gz` (or better, `gnucobol-2.0.tar.gz`)
- (For changing the compiler sources, `GNU Autoconf` tools are also required).

From a working directory, of your choice:

    prompt$ tar xvf gnucobol.tar.gz
    prompt$ cd gnucobol

To see the possible configuration options, use:

    prompt$ ./configure --help

Then

    prompt$ ./configure
    prompt$ make

Assuming dependencies are in place and the build succeeds, verify the pre-install with

    prompt$ make check

or

    prompt$ make checkall

That runs internal checks of the compiler (`make check`) and optionally runs tests against the NIST COBOL85 verification suite (`make checkall`).  Version 1.1 of OpenCOBOL covers some 9100 NIST tests, recent versions cover more than 9700 test passes. *The NIST COBOL85 testsuite is no longer maintained, but is a very comprehensive and respectable set of tests.  COBOL is highly backward compatible, by design intent, but new COBOL 2002 and COBOL 2014 features are not part of the NIST verification suite.*

The internal checks cover some 500 tests and sample code compiles.

If all is well, the last step is

    prompt$ sudo make install

Or, for systems without `sudo`, become the root user for `make install` or use a `./configure` prefix that does not require super user permissions.  The default prefix for source builds is `/usr/local`.

If more than one build has occurred on the machine, and local libraries are re-installed, this needs to be followed up with

    prompt$ sudo ldconfig

To ensure that the linker loader `ld` cache is properly refreshed to match the new compiler install.

`cobc` will be ready for use.

`cobc --help` for quick help, `info open-cobol` (or `info gnucobol`) for deeper help, and visit http://open-cobol.sourceforge.net/ for links to the Programmer's Guide and a 1200+ page FAQ document. 

Installation problems, issues or general questions can be posted to the GnuCOBOL project space, in the `Help getting started` Discussion pages on SourceForge.

