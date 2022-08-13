---
title: "Getting started with arangodb"
slug: "getting-started-with-arangodb"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation & Setup
These instructions are for a base install of Community Edition for local development purposes.

# Mac

<sup>(Official documentation is maintained [here][1])</sup>

## 1. Install

Dependencies: [Homebrew][2]


```
brew install arangodb
```

If you don't want Homebrew or otherwise prefer a binary, you can download it instead [here][3].

## 2. Run

```
/usr/local/sbin/arangod &
```

## 3. Try it out

Open your browser and point to [localhost:8529][4].

Run `arangosh` in your shell. (If it's not in your PATH, you can find this in 
`/usr/local/bin/arangosh`).


## 4. Tweak settings (optional)

The configuration file is located in `/usr/local/etc/arangodb3/arangod.conf
`.


----------


# Ubuntu 

<sup>(Official documentation is maintained [here][5])</sup>

If you just want the deb file and no package management, download it [here][6].

If you want to install for 14.04 instead of 16.04, change the version to 14.04 where you see 16.04 below.

## 1. Add the repository

```
wget https://www.arangodb.com/repositories/arangodb31/xUbuntu_16.04/Release.key
apt-key add - < Release.key
```

## 2. Install
```
echo 'deb https://www.arangodb.com/repositories/arangodb31/xUbuntu_16.04/ /' | sudo tee /etc/apt/sources.list.d/arangodb.list
sudo apt install apt-transport-https
sudo apt update
sudo apt install arangodb3
```

## 3. Run

```
sudo service arangodb3 start
```

## 4. Try it out
 
Open your browser and point to [localhost:8529][4].

Run `arangosh` in your shell.

## 5. Tweak settings

You can change ArangoDB to your heart's desire at `/etc/arangodb3/arangod.conf`.

----------


# Windows
<sup>(Official documentation is maintained [here][7])</sup>

Download [here][8], follow the wizard. 


----------

# Compile from source

Detailed compilation instructions for several platforms are available [here][9].


  [1]: https://docs.arangodb.com/3.1/Manual/GettingStarted/Installing/MacOSX.html
  [2]: https://brew.sh/
  [3]: https://www.arangodb.com/download-major/macosx/
  [4]: http://localhost:8529 "localhost:8529"
  [5]: https://docs.arangodb.com/3.1/Manual/GettingStarted/Installing/Linux.html
  [6]: https://www.arangodb.com/download-major/Ubuntu/
  [7]: https://docs.arangodb.com/3.0/Manual/GettingStarted/Installing/Windows.html
  [8]: https://www.arangodb.com/download-major/windows/
  [9]: https://docs.arangodb.com/3.1/cookbook/Compiling/

