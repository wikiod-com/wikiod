---
title: "Pipelines"
slug: "pipelines"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
- heroku pipelines:<install|create|promote>...

A pipeline is a group of Heroku apps that share the same codebase. Apps in a pipeline are grouped into “review”, “development”, “staging”, and “production” stages representing different deployment steps in a continuous delivery workflow.

## Pipelines via the CLI
**Installing pipeline**

Once Heroku Toolbelt is installed it requires [Pipelines plugin][1] too. 

    heroku plugins:install heroku-pipelines
---

**Creating pipelines**

 You must start with an app to add to the pipeline, although it doesn’t have to be for a particular stage. If you don’t specify `--stage STAGE`, the CLI will guess at the appropriate stage, but also let you override the default. The name of the pipeline will be guessed from the app name as well, but can be overridden either by adding the `NAME` on the command line, or entering a different name when prompted.

    heroku pipelines:create -a example
---

**Promoting**

The target app(s) will be automatically determined by the downstream stage

    heroku pipelines:promote -r staging

It is also possible to promote to a specific app (or set of apps)

    heroku pipelines:promote -r staging --to my-production-app1,my-production-app2
---

**Help Command**

A complete list of Pipelines commands with usage details is available in the console

    heroku help pipelines



  [1]: https://github.com/heroku/heroku-pipelines

