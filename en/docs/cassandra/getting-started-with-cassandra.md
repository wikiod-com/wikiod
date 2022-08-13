---
title: "Getting started with cassandra"
slug: "getting-started-with-cassandra"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
## Single node Installation ##
 1. Pre-install NodeJS, Python and Java
 2. Select your installation document based on your platform http://docs.datastax.com/en/cassandra/3.x/cassandra/install/installTOC.html
 3. Download Cassandra binaries from http://cassandra.apache.org/download/
 4. Untar the downloaded file to `<installation location>`
 5. Start the cassandra using `<installation location>/bin/cassandra` OR start Cassandra as a service - `[sudo] service cassandra start`
 6. Check whether cassandra is up and running using `<installation location>/bin/nodetool status`.
 
Ex:  
  1. On Windows environment run `cassandra.bat` file to start Cassandra server and `cqlsh.bat` to open CQL client terminal to execute CQL commands.

There are two ways that installation for a **Single Node** can be carried out.

You should have Oracle Java 8 or OpenJDk 8 (preferred for Cassandra versions > 3.0)

### 1. Installing a Debian package (installs Cassandra as a service)

Add the Cassandra version to the repository (replace the 22x with your own version for example for 2.7 use 27x)

<!-- language: bash -->

    echo "deb-src http://www.apache.org/dist/cassandra/debian 22x main" | sudo tee -a /etc/apt/sources.list.d/cassandra.sources.list
    # Update the repository  
    sudo apt-get update
    # Then install it
    sudo apt-get install cassandra cassandra-tools

Now Cassandra can be started and stopped using:

    sudo service cassandra start
    sudo service cassandra stop

Check the status using:

    nodetool status

Logs and Data directories are `/var/log/cassandra` and `/var/lib/cassandra` respectively.

### 2. Installing any version of Cassandra in form of binary tarball (installs Cassandra as a standalone process)

Download the Datastax version:

    curl -L  http://downloads.datastax.com/community/dsc-cassandra-version_number-bin.tar.gz | tar xz

Or Apache Cassandra binary tarball manually (from the site http://www.apache.org/dist/cassandra/)

Now untar this:

    tar -xvzf dsc-cassandra-version_number-bin.tar.gz

Change the directory to install location: 

    cd install_location

Start Cassandra using:

    sudo sh ./bin/cassandra 

Stop using:

    sudo kill -9 pid

Check: 

    ./bin/nodetool status

And viola, you have a single-node test cluster for Cassandra. So just use `cqlsh`
in the terminal for Cassandra shell.


Configuration of Cassandra can be done in `cassandra.yaml` in `conf` folder in `install_location`.


## Multi node installation ##


## Multi DC Cluster Installation ##


