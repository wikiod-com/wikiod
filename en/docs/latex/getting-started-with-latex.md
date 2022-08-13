---
title: "Getting started with latex"
slug: "getting-started-with-latex"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## LaTeX Editors
While you can create LaTeX documents using any editor and compiling using the console, there exist several plugins for widely used editors to simplify creating LaTeX documents, and there are specialized LaTeX editors. An [exhaustive list of LaTeX editors][1] is available on [TeX.SE][2] (the StackExchange site, dedicated to TeX, LaTeX & Friends).

The most widely used editors, according to this list, are:

- The [Emacs][3] editor with the [AUCTeX][4] extension.
- The [Vim][5] editor with the [LaTeX-suite][6] plugin.
- [Texmaker][7] – a specialized LaTeX IDE.
- [TeXstudio][8] – another LaTeX IDE.
- [TeXworks][9] – one more LaTeX IDE.

While experienced users of Emacs or Vim may want to stick to their editor (whose plugins provide a host of functionality unavailable elsewhere), a specialized IDE might be easier to install/use for beginners. The last three on the list have a preview function where one can see the results of the compilation of the document.

Additionally, there are online LaTeX tools that can be of use to beginners or people that must collaborate, e.g. [ShareLaTeX][10] and [Overleaf][11].


  [1]: http://tex.stackexchange.com/questions/339/latex-editors-ides
  [2]: http://tex.stackexchange.com/
  [3]: http://www.gnu.org/software/emacs/emacs.html
  [4]: http://www.gnu.org/software/auctex/
  [5]: http://www.vim.org/
  [6]: http://vim-latex.sourceforge.net/
  [7]: http://www.xm1math.net/texmaker/
  [8]: http://texstudio.sourceforge.net/
  [9]: https://github.com/TeXworks/texworks/releases
  [10]: https://www.sharelatex.com/
  [11]: https://www.overleaf.com/

## Installation and Setup
You can choose between major distributions of LaTeX:

 - [TeX Live][1] (Windows, Linux, and OS X), the standard, cross-platform distribution.
 - [MacTeX][2] (Mac) A packaged version of TeX Live made for OS X with some Mac-specific tools
 - [MiKTeX][3] (Windows) A separate distribution entirely that 

All distributions are more or less equivalent in an ideal world. TeX Live has the advantage of being available on all platforms and thus has much better community support. MiKTeX can take advantage of Windows-specific features. For licensing reasons, MiKTeX will also distribute a few packages that TeX Live will not.

In all cases, the full install is recommended.  Specifically, using MiKTeX's download-on-command feature will hang/crash many editors.

# Installation
## Windows (TeXLive)

 1. Download the most recent TeXLive `install-tl-windows.exe` from their [website][1].
 2. Run `install-tl-windows.exe` and follow the instructions.

## Windows (MiKTeX)

 1. Download the most recent MiKTeX installer from their [website][5].
 2. Run the installer and follow the instructions.

## Mac OS X (TeXLive)

 1. Download the most recent MacTeX from their [website][4].
 2. Run `MacTeX.pkg` and follow the instructions.

## Linux (TeXLive)

Linux users have two options:

1. Install via your distribution's package manager (usually several releases behind)
2. Install from upstream (released yearly, updated often)

### Using Package Managers

- Arch Linux: `pacman -S texlive-most`
- Debian/Ubuntu/Mint: `apt-get install texlive-full`
- Fedora: `yum install texlive`

Note that using this method means that you will be dependent on that package's maintainer for the distribution for updates.  These packages will often be several releases behind the most recent distribution, often meaning critical updates will be missing.  It's almost always best to install from upstream. Also note that the distribution's package manager will probably not recognize the direct installation and could try to install it when one installs other related support packages.

### Installing from Upstream

 1. Download the most recent TeXLive `install-tl-unx.tar.gz` from their [website][1].
 2. Extract the files from the archive with `tar -zxvf install-tl-unx.tar.gz`.
 3. Change into the downloaded folder with `cd install-tl-unx`.
 4. Run `./install-tl` and follow the instructions.

    TeXLive should now be installed under `/usr/local/texlive/YEAR/`, where `YEAR` is the four digit year (e.g. `2016`). In this way, it is possible to have multiple TeXLive versions alongside each other and switch between them by changing your PATH variable.

    Open this folder and check the `bin` folder. It should contain a subfolder, which (depending on your platform) will be something like `i386-linux` or `x86_64-linux`.

 5. Add the TeX Live binary folder to your path with

        EXPORT PATH=/usr/local/texlive/YEAR/bin/PLATFORM:$PATH

    where `YEAR` is the four digit year (e.g. `2016`), and `PLATFORM` is your platform (e.g. `x86_64-linux`).

# Test Installation

The LaTeX installation is now complete. To test it, create a new file with your favorite text editor, name it `test.tex` and add the following content:

```latex
\documentclass{article}
\begin{document}
  Hello World!
\end{document}
```

Now, open the console or terminal, navigate to the folder where you saved `test.tex` and run

```sh
pdflatex test
```

(Note that your editor may have facilities to run this for you.)

This creates several new files, including `test.pdf`. This is the output document, and looks like this:

[![Resulting PDF file from compiling `test.tex][6]][6]

Congratulations, you have successfully installed LaTeX, and created your first LaTeX document!

  [1]: https://www.tug.org/texlive/
  [2]: http://tug.org/mactex/
  [3]: http://miktex.org/
  [4]: http://www.tug.org/mactex/mactex-download.html
  [5]: http://miktex.org/download
  [6]: http://i.stack.imgur.com/fIs4k.png


