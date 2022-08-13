---
title: "Command Line"
slug: "command-line"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

The Heroku Command Line Interface (CLI), formerly known as the Heroku Toolbelt, is a tool for creating and managing Heroku apps from the command line / shell of various operating systems.

## Syntax
- $ heroku --version
- $ heroku login
- $ heroku create

## Download and install
# OS X
Download and run the [OS X installer][1].
# Windows
Download and run the Windows installer [32-bit][2] [64-bit][3].
# Debian/Ubuntu
Run the following to add our apt repository and install the CLI:

    $ sudo add-apt-repository "deb https://cli-assets.heroku.com/branches/stable/apt ./"
    $ curl -L https://cli-assets.heroku.com/apt/release.key | sudo apt-key add -
    $ sudo apt-get update
    $ sudo apt-get install heroku
# Standalone version
Download the tarball and extract it so that you can access the binary from your PATH. For example:

    $ echo replace OS/ARCH with values as noted below
    $ wget https://cli-assets.heroku.com/branches/stable/heroku-OS-ARCH.tar.gz
    $ tar -xvzf heroku-OS-ARCH /usr/local/lib/heroku
    $ ln -s /usr/local/lib/heroku/bin/heroku /usr/local/bin/heroku

# Verify your installation
To verify your CLI installation use the `heroku --version` command.

    $ heroku --version
    heroku-cli/5.6.0-010a227 (darwin-amd64) go1.7.4

  [1]: https://cli-assets.heroku.com/branches/stable/heroku-osx.pkg
  [2]: https://cli-assets.heroku.com/branches/stable/heroku-windows-386.exe
  [3]: https://cli-assets.heroku.com/branches/stable/heroku-windows-amd64.exe

## Getting started
You will be asked to enter your Heroku credentials the first time you run a command; after the first time, your email address and an API token will be saved to `~/.netrc` for future use.

It’s generally a good idea to login and add your public key immediately after installing the Heroku CLI so that you can use git to push or clone Heroku app repositories:

    $ heroku login
    Enter your Heroku credentials.
    Email: adam@example.com
    Password (typing will be hidden):
    Authentication successful.

You’re now ready to create your first Heroku app:

    $ cd ~/myapp
    $ heroku create
    Creating app... done, ⬢ sleepy-meadow-81798
    https://sleepy-meadow-81798.herokuapp.com/ | https://git.heroku.com/sleepy-meadow-81798.git

