---
title: "Command line interface (CLI)"
slug: "command-line-interface-cli"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Connecting to the local Wildfly server
To connect to a local Wildfly server via the command line, the tool `bin/jboss-cli.sh` can be used:

    $ ./bin/jboss-cli.sh --connect
    [standalone@localhost:9990 /] 

To connect to a remote Wildfly server, use the `--controller` option:

    $ ./bin/jboss-cli.sh  --connect --controller=localhost:9990
    [standalone@localhost:9990 /] 

Note that the port `9990` is used for the administrative operations, and as such, should *not* be opened to the public internet. By default, Wildfly binds the administrative port to `localhost`, which means that it cannot be accessed from remote hosts. To overcome this, start Wildfly with the `-bmanagement` option, specifying the IP it should bind to:

    $ ./bin/standalone.sh -bmanagement 0.0.0.0

