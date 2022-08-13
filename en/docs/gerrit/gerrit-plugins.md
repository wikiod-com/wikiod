---
title: "Gerrit Plugins"
slug: "gerrit-plugins"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

* Gerrit plugins are not provided compiled. Sources can be download from [Gerrit Plugin code site][1]. Nevertheless, you can find **compiled version** on the [community Gerrit compiled plugins website][2].

* Install plugin : https://gerrit-review.googlesource.com/Documentation/cmd-plugin-install.html

  [1]: https://gerrit.googlesource.com/plugins/
  [2]: http://builds.quelltextlich.at/gerrit/nightly/master/latest-ok/index.html

## Replication plugin
It is used to duplicate a git repository from gerrit to anyway. Configuration file is `$GERRIT_INSTALL/etc/replication.config`.

Config file example to clone `MyRepo` from `gerrit` to `backupServer`


    [remote "backup"]
        url = ProjectUrlOnBackupServer/${name} #Example backup.some.org:/pub/git/${name}.git
        push = +refs/heads/*:refs/heads/*
        push = +refs/tags/*:refs/tags/*
        projects = MyRepo

To reload the plugin, restart Gerrit server or execute following command if `install plugin` is installed:

    ssh -p 29418 localhost gerrit plugin reload replication

Full documentation : [Official Replication Plugin Documentation][1]


  [1]: https://gerrit.googlesource.com/plugins/replication/+doc/master/src/main/resources/Documentation/config.md

## Events-log plugin
This plugin is mandatory to make Gerrit able to receive message from other services. As an example, it has to be install to use `Sonar Gerrit` plugin

* Install `jar` file under `plugins` folder
* Default configuration is sufficient


