---
title: "Color script output (cross-platform)"
slug: "color-script-output-cross-platform"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

**`tput`** queries the terminfo database for terminal-dependent information.

From [tput on Wikipedia](https://en.wikipedia.org/wiki/Tput):

> In computing, `tput` is a standard Unix operating system command which makes use of terminal capabilities.
>
> Depending on the system, `tput` uses the terminfo or termcap database, as well as looking into the environment for the terminal type.

from [Bash Prompt HOWTO: Chapter 6. ANSI Escape Sequences: Colours and Cursor Movement](http://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/x405.html):

* **tput setab [1-7]**

  * Set a background colour using ANSI escape

* **tput setb [1-7]**

  * Set a background colour

* **tput setaf [1-7]**

  * Set a foreground colour using ANSI escape

* **tput setf [1-7]**

  * Set a foreground colour

* **tput bold**

  * Set bold mode

* **tput sgr0**

  * Turn off all attributes (doesn't work quite as expected)

## color-output.sh
In the opening section of a bash script, it's possible to define some variables that function as helpers to color or otherwise format the terminal output during the run of the script.

Different platforms use different character sequences to express color. However, there's a utility called `tput` which works on all *nix systems and returns platform-specific terminal coloring strings via a consistent cross-platform API.

For example, to store the character sequence which turns the terminal text red or green:

```sh
red=$(tput setaf 1)
green=$(tput setaf 2)
```

Or, to store the character sequence which resets the text to default appearance:

```sh
reset=$(tput sgr0)
```

Then, if the BASH script needed to show different colored outputs, this can be achieved with:

```
echo "${green}Success!${reset}"
echo "${red}Failure.${reset}"
```

