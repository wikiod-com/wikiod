---
title: "Event Listeners in Forge"
slug: "event-listeners-in-forge"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Creating an event listener in Forge
Creating an event listener in Forge is very similar to creating one in Bukket.

Creating the listener class requires a lot less. There's no interface to implement or other imports.


    public class ListenerClass { } //perfectly valid event listener class

Registering it requires passing the instance to the Forge event bus:

    MinecraftForge.EVENT_BUS.register(new ListenerClass());

There are a couple of different event buses depending on the event. For example, ore generation events are fired on the `ORE_GEN_BUS`.  You can call this registration from anywhere, but it is advised to call it from either your main mod class (with the @Mod annotation) or from a proxy class (some events are client side only and a client side event handler must only be called from the client proxy, otherwise the dedicated server will crash!)

To listen to any given event in your listener class, you must create a method with @SubscribeEvent annotation on the method. The event type is specified by the Type in the method's only argument. The method may be named whatever you wish.

Note that some event types are subtypes (which should be referenced by their enclosing type, e.g. `CropGrowEvent.Pre`) and that some events may have a Phase as it is fired in more than one place (such as all `TickEvent`s which are fired both before and after all vanilla code). As a modder you should always check for both of these things and only run your code when needed.

    public class ListenerClass {
        @SubscribeEvent
        public void onPlayerLogin(PlayerLoggedInEvent event) {
            event.player.addChatMessage(new TextComponentString("Welcome to the server!"));
        }
    }

As Forge mods interact directly with Minecraft internals, a lot of power is given to the modder to be able to affect things, but similarly, the code must follow the vanilla framework: there's no shortcuts for sending messages, the message has to be constructed from ITextComponents manually, but the ability to manipulate these objects (such as applying color formatting) is much easier.  For example:

    TextComponentString txt = new TextComponentString(
        TextFormatting.LIGHT_PURPLE + "Welcome to the server!");
    txt.appendSibling(new TextComponentString(
        TextFormatting.AQUA + "Server has been online for " + x + " days"));
    event.player.addChatMessage(txt);

Which gives the following result:
[![Chat message][1]][1]


  [1]: https://i.stack.imgur.com/wj5Dy.png

