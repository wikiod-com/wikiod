---
title: "Patterns"
slug: "patterns"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Regexp Patterns
Patterns can be specified as regular expressions:

    /regular expression/ {action}

For example:

    echo "user@hostname.com
    not an email" | awk '/[^@]+@.+/ {print}'

Produces: 

    user@hostname.com

Note that an action consisting only of the `print` statement can be omitted entirely. The above example is equivalent to:

    echo "user@hostname.com
    not an email" | awk '/[^@]+@.+/'


