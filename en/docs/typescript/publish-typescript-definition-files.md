---
title: "Publish TypeScript definition files"
slug: "publish-typescript-definition-files"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Include definition file with library on npm
Add typings to your package.json

    {
    ...
    "typings": "path/file.d.ts"
    ...
    }

Now when ever that library is imported typescript will load the typings file

