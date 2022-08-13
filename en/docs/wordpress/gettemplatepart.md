---
title: "get_template_part()"
slug: "get_template_part"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
 - get_template_part('file-name-no-extension');

## Parameters
| Parameter | Description |
| ------ | ------ |
| file-name-no-extension   | The name of the template part with no extension. E.g. 'foo' instead of 'foo.php'   |

## Loading Template Part
Pulls the code from a certain specified file into another file where the call was made.

E.g. 
inside example.php

    <h1>Hello World!</h1>

Inside page.php

    // header code
    get_template_part('example');
    // rest of page code

Output:

    // header code
    <h1>Hello World</h1>
    // rest of page code

