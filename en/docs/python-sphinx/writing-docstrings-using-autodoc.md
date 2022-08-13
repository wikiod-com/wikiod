---
title: "Writing docstrings using autodoc"
slug: "writing-docstrings-using-autodoc"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Sphinx allows the inclusion of docstrings in the documentation using the [autodoc extension](http://www.sphinx-doc.org/tutorial.html#autodoc) which is shipped with the package. This documentation will show you how to format your docstrings an how to include them in your documentation.

## Installing the autodoc extension
Add the autodoc module in the `extensions` `list` present in the `conf.py` file at the root of your documentation: 

    extensions = [
        'sphinx.ext.autodoc',
         ...
    ]



## Adding your code path in the sphinx config
Autodoc needs to imports your modules to work.

You can include your code path in your `conf.py` file.

For instance:

    import os
    sys.path.insert(0, os.path.abspath('../src'))




