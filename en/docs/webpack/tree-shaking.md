---
title: "Tree Shaking"
slug: "tree-shaking"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## ES2015 tree shaking
webpack 2 introduces tree shaking which can remove unused code when ES2015 modules are used to import and export code.

## Install
    npm install babel-preset-es2015-webpack --save-dev
## Usage
in .babelrc:

    {
        "presets": [
            "es2015-webpack"
        ]
    }

