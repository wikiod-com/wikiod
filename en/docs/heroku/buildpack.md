---
title: "Buildpack"
slug: "buildpack"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Multiple buildpacks
An application can also contain more than one buildpack. It can be achieved using `add`:

    heroku buildpacks:add --index 1 <buildpack_name>

where, `--index` parameter specifies the execution order of buildpack. 

Say,

    heroku buildpacks:set heroku/php
    heroku buildpacks:add --index 1 heroku/nodejs

will set the buildpack order as:

    heroku/nodejs
    heroku/php

---

**Remember:** A Heroku app has only one public port - 80. Hence either of the one will serve in one port. Say, if `procfile` is specified with `web: node server.js`, node application will run in port 80, otherwise PHP. However, the build will run in the order specified. If one needs more than one application, set up multiple projects and make it to communicate with each other.

## Setting Buildpacks
Heroku officially supports [buildpacks][1] for Ruby, Node.js, Clojure, Python, Java, Gradle, Grails, Scala, Play, PHP and Go.

Buildpacks are automatically detected by Heroku in the above order, however, it can also be set manually through CLI using:

1. At the time of app creation
        
        heroku create <app_name> --buildpack <buildpack_name>

2. Manually,

        heroku buildpacks:set <buildpack_name>

---

Buildpack name can be specified either using shorthand or URL. Like for PHP buildpack,

    heroku buildpacks:set heroku/php
or

    heroku buildpacks:set https://elements.heroku.com/buildpacks/heroku/heroku-buildpack-php

  [1]: https://devcenter.heroku.com/articles/buildpacks#officially-supported-buildpacks

