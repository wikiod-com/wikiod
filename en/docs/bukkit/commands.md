---
title: "Commands"
slug: "commands"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Syntax
 - `@Override public boolean onCommand(CommandSender sender, Command cmd, String label, String[] args)`

## Handling a Command
To handle a command, you must have a class that implements the CommandExecutor interface. The JavaPlugin class (your plugin's main class) already implements this.

When implementing the CommandExecutor interface, the following method must be implemented:

    public boolean onCommand(CommandSender sender, Command cmd, String label, String[] args) {
    //Handle your command in here
    return true;      ///Should return false if you want to show the usage
    
    }

Sender is the one who sent the command. It can be a Player or the Console.


CMD is the command you're listening to, as declared in plugin.yml. Not to be confused with label.

label is the alias used to execute this command, it's what the sender types after the slash.

and finally, args are the arguments the sender may have used to send your command.




A possible command might go as 

> /tell Kerooker Hi, Kerooker!

Tell would be your label, and may also be defined as your command if you said so in plugin.yml;

'Kerooker', 'Hi,' , 'Kerooker!' are your args 0, 1 and 2, respectively

As a return, you will probably always want to return true when you expected everything to happen that way. You should return false if you want to show the sender the the command usage defined in your plugin.yml

## A simple set GameMode command ( /gm <gamemode> )
This example shows a very basic example of how to utilize onCommand. I don't suggest processing your commands directly in onCommand, but this does the trick for this simple case. 

In this example we attempt to set the player's gamemode.

The first thing we need to do is make sure that the sender isn't a ConsoleCommandSender, because we can't set a console's gamemode. This is done with (sender instanceof Player).

Next, we want the player to type /gm CREATIVE (or what ever other gamemode) so we have to check 2 things:

 1. make sure that they pass in 1 argument (CREATIVE)
 2. make sure that their command was "gm"

We accomplished these checks with: args.length == 1 && label.equalsIgnoreCase("gm")

Now we know for sure the player typed "/gm x".

The next thing we need to do is turn args[0] into a GameMode object so that we can apply it to the player. This can be done with GameMode.valueOf(String)
However, according to the Java enumeration documentation, if a string is passed into valueOf() that doesn't match an enumeration, it will throw an IllegalArgumentException - so we have to make sure to catch that.

Once we have the gamemode, we can go ahead and simply use p.setGameMode(gm) and the player's gamemode will change. In the case that we caught an exception - we simply print out a statement and return false.

 

    @Override
    public boolean onCommand(CommandSender sender, Command command, String label, String[] args) {
        if (sender instanceof Player) { 
            final Player p = (Player) sender;

            if (args.length == 1 && label.equalsIgnoreCase("gm")) {
                try {
                    GameMode gm = GameMode.valueOf(args[0]);
                    p.setGameMode(gm);
                    p.sendMessage(ChatColor.GREEN + "Your gamemode has been set to: " + gm.toString());
                    return true;
                } catch (IllegalArgumentException e) {
                    p.sendMessage(ChatColor.RED + "Invalid gamemode option!");
                    return false;
                }

            }
        }
        return false;
    }

## Command not in main class
If you have a lot of commands, you shouldn't put them all in the main class. 
1. Make a new class and have it implement `CommandExecutor`
2. Add the following to the class:
       
        @Override 
        public boolean onCommand(CommandSender sender, Command cmd, String label, String[] args) {

        }
3. In your main class add in the onEnable (replace commandName with the name of the command and CommandExecutor with the name of the class):
    
        getCommand("commandName").setExecutor(new CommandExecutor());

