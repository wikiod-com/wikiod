---
title: "Installation and Setup"
slug: "installation-and-setup"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Check if Redis is working      
    redis-cli ping

This should return `PONG`


## Access Redis Cli
Assuming that you are running redis server on localhost you can type command
``` 
redis-cli
```
After this command appear redis command line prompt
``` 
127.0.0.1:6379>
```


## Redis data types
The following is the list of all the data structures supported by Redis:
- **Binary-safe strings**
- **Lists**: collections of string elements sorted according to the order of insertion. 
- **Sets**: collections of unique, unsorted string elements.
- **Sorted sets**: similar to Sets but where every string element is associated to a floating number value, called score. 
- **Hashes**: are maps composed of fields associated with values.
- **HyperLogLogs**: this is a probabilistic data structure which is used in order to estimate the cardinality of a set. 

*Based on **redis.io** official documentation*

## Installing Redis

    wget http://download.redis.io/redis-stable.tar.gz
    tar xvzf redis-stable.tar.gz
    cd redis-stable
    make


## Starting Redis
    redis-server


## Installing and running Redis Server on Windows


*Note:* The Redis project does not officially support Windows.

 However, the **Microsoft Open Tech group** develops and maintains this Windows port targeting Win64. **[Official redis.io/download][1]**

You can choose to download different versions or the latest version of Redis [ github.com/MSOpenTech/redis/releases][2]


1.  **Download** either .msi or .zip file, this tutorial will let you download latest zip file  
[**Redis-x64-3.2.100.zip**][3].
2. **Extract the zip file** to  prepared directory.
[![enter image description here][4]][4]
3. **Run redis-server.exe**,   you can either directly run redis-server.exe by clicking or  run via command prompt.
[![enter image description here][5]][5]
4. **Run redis-cli.exe**, after successfully running the redis-server. You can access it and test commands by running redis-cli.exe
Te

  [![enter image description here][6]][6]

 **PING** command is used to test if a connection is still alive.
    [![enter image description here][7]][7]


You can now start  using Redis , please refer for more [commands in official documentations][8]  


  [1]: https://redis.io/download
  [2]: https://github.com/MSOpenTech/redis/releases
  [3]: https://github.com/MSOpenTech/redis/releases/download/win-3.2.100/Redis-x64-3.2.100.zip
  [4]: https://i.stack.imgur.com/7mhpp.png
  [5]: https://i.stack.imgur.com/lKqjc.png
  [6]: https://i.stack.imgur.com/BenT1.png
  [7]: https://i.stack.imgur.com/MA66K.png
  [8]: https://redis.io/commands

