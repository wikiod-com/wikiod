---
title: "Setting up Scala"
slug: "setting-up-scala"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Ubuntu Installation via Manual Download and Configuration
Download your preferred version from [Lightbend][1] with [`curl`][2]:

```
curl -O http://downloads.lightbend.com/scala/2.xx.x/scala-2.xx.x.tgz
```

Unzip the `tar` file to `/usr/local/share` or `/opt/bin`:

```
unzip scala-2.xx.x.tgz 
mv scala-2.xx.x /usr/local/share/scala
```

Add the `PATH` to `~/.profile` or `~/.bash_profile` or `~/.bashrc` by including this text to one of those files:
```
$SCALA_HOME=/usr/local/share/scala
export PATH=$SCALA_HOME/bin:$PATH
```

To verify that it is installed correctly, in the terminal command prompt:
```
which scala
```
The response returned should be the equivalent to what you placed in your `PATH` variable. To verify that `scala` is working:
```
scala
```
This should start the Scala REPL, and report the version (which, in turn, should match the version you downloaded).


  [1]: http://www.lightbend.com/
  [2]: https://en.wikipedia.org/wiki/CURL

## On Linux via dpkg

On Debian-based distributions, including Ubuntu, the most straightforward way is to use the `.deb` installation file. Go to the [Scala website][1]. Choose the version you want to install then scroll down and look for `scala-x.x.x.deb`.

You can install the scala deb from command line:

    sudo dpkg -i scala-x.x.x.deb

To verify that it is installed correctly, in the terminal command prompt:

    which scala
The response returned should be the equivalent to what you placed in your PATH variable. To verify that scala is working:

    scala
This should start the Scala REPL, and report the version (which, in turn, should match the version you downloaded).

  [1]: http://www.scala-lang.org/download/all.html

## Mac OSX via Macports
On Mac OSX computers with [MacPorts][1] installed, open a terminal window and type:

```
port list | grep scala
```

This will list all the Scala-related packages available. To install one (in this example the 2.11 version of Scala):

```
sudo port install scala2.11
```

(The `2.11` may change if you want to install a different version.)

All dependencies will automatically be installed and your `$PATH` parameter updated. To verify everything worked:

```
which scala
```
This will show you the path to the Scala installation.

```
scala
```
This will open up the Scala REPL, and report the version number installed.


  [1]: https://www.macports.org/

