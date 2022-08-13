---
title: "Getting started with rethinkdb"
slug: "getting-started-with-rethinkdb"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation on Windows
## Downloading 

_Prerequisites:_ We provide native 64-bit binaries for Windows 7 and above. A 64-bit version of Windows is required.

[Download](https://download.rethinkdb.com/windows/rethinkdb-{{site.version.full}}.zip) the ZIP archive and unpack it in a directory of your choice.

The Windows port of RethinkDB is a recent addition and hasn't received as much tuning as the Linux and OS X versions yet. Please report any performance issues on [GitHub][gh-issues].

[gh-issues]: https://github.com/rethinkdb/rethinkdb/issues/

## Running RethinkDB

The Windows version of RethinkDB, like the Linux/OS X versions, is executed from the command line. You'll need to start the Windows command shell.

* Press `Win` + `X` and click "Command Prompt"; or
* Open the Start Menu, click "Run," and type "cmd" `ENTER`

Use the `cd` command to go to the directory that you unpacked `rethinkdb.exe` in.

    C:\Users\Slava\>cd RethinkDB
    C:\Users\Slava\RethinkDB\>

Then, you can start RethinkDB with its default options.

    C:\Users\Slava\RethinkDB\>rethinkdb.exe

You can also use any of the [command line options][cl] to control configuration (as well as specify a [configuration file][cf]).

[cl]: /docs/cli-options/
[cf]: /docs/config-file/

To start with a specific data directory:

    rethinkdb.exe -d c:\RethinkDB\data\

To specify a server name and another cluster to join:

    rethinkdb.exe -n jarvis -j cluster.example.com

## Installation on OS X
## Using Homebrew 

_Prerequisites:_ Make sure you're on OS X 10.9 (Mavericks) or above, and
have [Homebrew](http://mxcl.github.com/homebrew/) installed.

Run the following in your terminal:

```bash
brew update && brew install rethinkdb
```

## Compile from source 

Building RethinkDB from source requires OS X 10.9 (Mavericks) or greater. [Xcode](https://developer.apple.com/xcode/) is required to
build from source.

### Get the source code 

Download and extract the archive:

```bash
wget https://download.rethinkdb.com/dist/rethinkdb-2.3.4.tgz
tar xf rethinkdb-2.3.4.tgz
```

### Build RethinkDB

Kick off the build process:

```bash
cd rethinkdb-2.3
./configure --allow-fetch --fetch openssl
make
```

You will find the `rethinkdb` binary in the `build/release/` subfolder.



## Installation on Ubuntu
Ubuntu binaries are available for both 32-bit and 64-bit architectures

```
source /etc/lsb-release && echo "deb http://download.rethinkdb.com/apt $DISTRIB_CODENAME main" | sudo tee /etc/apt/sources.list.d/rethinkdb.list
wget -qO- https://download.rethinkdb.com/apt/pubkey.gpg | sudo apt-key add -
sudo apt-get update
sudo apt-get install rethinkdb
```

