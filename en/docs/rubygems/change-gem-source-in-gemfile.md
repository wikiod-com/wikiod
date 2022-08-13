---
title: "Change gem source in Gemfile"
slug: "change-gem-source-in-gemfile"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Use github repo for specific gem in Gemfile
Instead of

`gem 'rails'`

You can specify a github user/repo combination with

`gem 'rails', github: 'rails/rails'`

## Make use of ref, branch, git for specific gem in gemfile
     gem 'any gem',git: 'any repo',branch: 'specific branch of that repo',ref: 'reference no.'

**ref** specifies individual commit.
**branch** specifies the git branch to pull from.


