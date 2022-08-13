---
title: "Basic .gitignore for elixir program"
slug: "basic-gitignore-for-elixir-program"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

Note that the `/rel` folder may not be needed in your .gitignore file. This is generated if you are using a release management tool such as `exrm`

## Example
```
### Elixir ###
/_build
/cover
/deps
erl_crash.dump
*.ez

### Erlang ###
.eunit
deps
*.beam
*.plt
ebin
rel/example_project
.concrete/DEV_MODE
.rebar


## Phoenix application
    /_build
    /db
    /deps
    /*.ez
    erl_crash.dump
    /node_modules
    /priv/static/
    /config/prod.secret.exs
    /rel

## Standalone elixir application
    /_build
    /cover
    /deps
    erl_crash.dump
    *.ez
    /rel



## Auto-generated .gitignore


## A basic .gitignore for Elixir


