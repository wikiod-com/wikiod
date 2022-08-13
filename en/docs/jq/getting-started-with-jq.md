---
title: "Getting started with jq"
slug: "getting-started-with-jq"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting jq set up or installed.

# Install on Mac

Using `brew`:

    $ brew install jq

Then test:

    $ echo '{"name": "john"}'
    {"name": "john"}

    $ echo '{"name": "john"}' | jq '.'
    {
      "name": "john"
    }
    
    $ echo '{"name": "john"}' | jq '.name'
    "john"

