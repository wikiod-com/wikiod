---
title: "Creating Commands"
slug: "creating-commands"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Subcommands
This example shows how to define sub-commands for a given command, and how to easily associate a command handler.  This example defines a `thing` command with `get` and `set` subcommands.

```java
package things;

import io.dropwizard.cli.Command;
import io.dropwizard.setup.Bootstrap;
import net.sourceforge.argparse4j.inf.FeatureControl;
import net.sourceforge.argparse4j.inf.Namespace;
import net.sourceforge.argparse4j.inf.Subparser;

public class ThingCommand extends Command {

    private static final String THING_COMMAND_NAME = "thing";

    private static final String THING_SUBCOMMAND_GET = "get";
    private static final String THING_SUBCOMMAND_SET = "set";
    
    public ThingCommand() {
        super(THING_COMMAND_NAME, "Thing management.");
    }

    @Override
    public void configure(Subparser subparser) {
        addSubCommand(subparser.addSubparsers().addParser(THING_SUBCOMMAND_GET)
            .help("Gets a thing."), new GetCommand());

        addSubCommand(subparser.addSubparsers().addParser(THING_SUBCOMMAND_SET)
            .help("Sets a thing."), new SetCommand());
    }

    private void addSubCommand(Subparser parser, Command cmd) {
        // associate the subcommand with a Command object
        parser.addArgument("--subcommand").help(FeatureControl.SUPPRESS).setDefault(cmd);
        parser.description(cmd.getDescription());
        cmd.configure(parser);
    }

    @Override
    public void run(Bootstrap<?> bootstrap, Namespace namespace) throws Exception {
        Command cmd = namespace.get("subcommand");
        assert cmd != null;
        cmd.run(bootstrap, namespace);
    }

    class GetCommand extends Command {

        public GetCommand() {
            super(THING_SUBCOMMAND_GET, "Gets a thing.");
        }

        @Override
        public void configure(Subparser cmd) {
            super.configure(cmd);
            cmd.addArgument("--name").type(String.class).required(true).help("Name of the thing");
        }

        @Override
        public void run(Bootstrap<?> bootstrap, Namespace namespace) throws Exception {
            String thingName = namespace.getString("name");
            System.out.println("Getting the thing named: " + thingName);
        }
    }

    class SetCommand extends Command {
        // ...   
    }
}
```

Here's an example invocation of the command (assuming an app named `myapp`):

```
$ bin/myapp thing get --name mything
Getting the thing named: mything
```


