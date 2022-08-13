---
title: "How to debug when docker build fails"
slug: "how-to-debug-when-docker-build-fails"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

When a `docker build -t mytag .` fails with a message such as

`---> Running in d9a42e53eb5a`

`The command '/bin/sh -c returned a non-zero code: 127` 

(127 means "command not found, but 
1) it is not trivial for everybody
2) 127 may be replaced by 6 or anything)

it may be non trivial to find the error in a long line

## basic example
As the last layer created by 

`docker build -t mytag .`

showed

`---> Running in d9a42e53eb5a`

You just launch the last created image with a shell and launch the command, and you will have a more clear error message

`docker run -it d9a42e53eb5a /bin/bash`

(this assumes /bin/bash is available, it may be /bin/sh or anything else)

and with the prompt, you launch the last failing command, and see what is displayed 

