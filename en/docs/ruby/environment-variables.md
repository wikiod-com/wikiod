---
title: "Environment Variables"
slug: "environment-variables"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 * ENV[variable_name]
 * ENV.fetch(variable_name, default_value)

Let get user profile path in a dynamic way for scripting under windows

## Sample to get user profile path
    # will retrieve my home path
    ENV['HOME'] # => "/Users/username"

    # will try retrieve the 'FOO' environment variable. If failed, will get 'bar'
    ENV.fetch('FOO', 'bar')

