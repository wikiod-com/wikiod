---
title: "Event Listeners in Bukkit"
slug: "event-listeners-in-bukkit"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Creating an event listener
To register your methods, the class containing the EventHandler(s) must implement the Listener interface.

    import org.bukkit.event.Listener;

    public final class ListenerClass implements Listener {
    }
You need to register the event listener by adding the following call to your onEnable method in the class that extends JavaPlugin:

    getServer().getPluginManager().registerEvents(new ListenerClass(), this);

To listen to any given event in your listener class, you must create a method with @EventHandler annotation on the method. The event type is specified by the Type in the method's only argument. The method may be named whatever you wish.

    import org.bukkit.event.Listener;
    import org.bukkit.event.EventHandler;
    import org.bukkit.event.player.PlayerLoginEvent;  

    public class ListenerClass implements Listener {
        @EventHandler
        public void onPlayerLogin(PlayerLoginEvent event) {
            event.getPlayer().sendMessage("Welcome to the server!");
        }
    }

## EventHandler Parameters
The `org.bukkit.event.EventHandler` annotation accepts a couple parameters.

**priority** - Indicates the priority of your listener. There are the six different priorities, in order of execution: LOWEST,LOW,NORMAL[default],HIGH,HIGHEST,MONITOR. These constants refer to the `org.bukkit.event.EventPriority` enum.

If you want to change the outcome of an event, choose very carefully from LOWEST to HIGHEST. Suggested generalized protection plugins on LOWEST, more specific plugins on NORMAL, and override plugins on HIGH. If you want to act when an event happens, but not change the outcome, use MONITOR.

**Note: The MONITOR priority should only be used for reading only. This priority is useful for logging plugins to see the results of an event and modifying values may interfere with those types of plugins.**

**ignoreCancelled** - A boolean which indicates whether or not your listener should fire if the event has been cancelled before it is the listener's turn to handle the event. False by default.

    import org.bukkit.event.Listener;
    import org.bukkit.event.EventHandler;
    import org.bukkit.event.EventPriority;
    import org.bukkit.event.player.PlayerLoginEvent;

    public final class LoginListener implements Listener {
        @EventHandler
        public void normalLogin(PlayerLoginEvent event) {
            // Some code here
        }    

        @EventHandler(priority = EventPriority.HIGH)
        public void highLogin(PlayerLoginEvent event) {
            // Some code here
        }
    }

## Creating Custom Events
Sometimes you need to create your own Event, one that other plugins can listen to (Vault, among other plugins, does this) and even cancel. Bukkit's Event API allows this to be possible. All you need to do is make a new class, have it extend `Event`, add the handlers and the attributes your event needs (like Player or message).

    import org.bukkit.event.Event;
    import org.bukkit.event.HandlerList;

    public final class CustomEvent extends Event {
        private static final HandlerList handlers = new HandlerList();
        private String message;

        public CustomEvent(String example) {
            message = example;
        }

        public String getMessage() {
            return message;
        }

        public HandlerList getHandlers() {
            return handlers;
        }
    
        public static HandlerList getHandlerList() {
            return handlers;
        }
    }

Calling your Custom Event
=========================

You are in control of creating and calling your events, where you call it is completely up to you. Here's an example

    // Create the event here
    CustomEvent event = new CustomEvent("Sample Message");
    // Call the event
    Bukkit.getServer().getPluginManager().callEvent(event);
    Bukkit.getServer().broadcastMessage(event.getMessage());

Remember: You are in control of your events. If you don't call it, and act upon it, it doesn't happen!

Listening to a Custom Event
===========================

Listening to a custom event is the same as listening to a normal event.

    import org.bukkit.event.Listener;
    import org.bukkit.event.EventHandler;

    public final class CustomListener implements Listener {
        
        @EventHandler
        public void onCustomEvent(CustomEvent event) {
        // Some code here
        }
    }

Making your CustomEvent Cancellable
===================================

If you ever want to make your event cancellable, just add `implements Cancellable`, `boolean cancelled` and a getter and setter:

    import org.bukkit.event.Event;
    import org.bukkit.event.HandlerList;
    import org.bukkit.event.Cancellable;

    public final class CustomEvent extends Event implements Cancellable {
        private static final HandlerList handlers = new HandlerList();
        private String message;
        private boolean cancelled;

        public CustomEvent(String example) {
            message = example;
        }

        public String getMessage() {
            return message;
        }

        public boolean isCancelled() {
            return cancelled;
        }

        public void setCancelled(boolean cancel) {
            cancelled = cancel;
        }

        public HandlerList getHandlers() {
            return handlers;
        }
    
        public static HandlerList getHandlerList() {
            return handlers;
        }
    }

Afterwards, you would check if a plugin had cancelled the custom event.

    // Create the event here
    CustomEvent event = new CustomEvent("Sample Message");
    // Call the event
    Bukkit.getServer().getPluginManager().callEvent(event);
    // Check if the event is not cancelled
    if (!event.isCancelled()) {
        Bukkit.getServer().broadcastMessage(event.getMessage());
    }

## Un-registering events or listeners
You can un-register individual events, entire listener classes or all events registered by your plugin or even by other plugins!

Un-register specific event
==========================
Each event class has the getHandlerList() static method, call that and then you can use .unregister() method. 

    PlayerInteractEvent.getHandlerList().unregister(plugin);
    // this will unregister all PlayerInteractEvent instances from the plugin
    // you can also specify a listener class instead of plugin.
Now you know why you'll need the getHandlerList() in your custom events.

Un-register all events
======================
Using the HandlerList class and its unregisterAll() static method you can easily unregister events from listener classes or plugins.

    HandlerList.unregisterAll(plugin);
    // this will unregister all events from the specified plugin
    // you can also specify a listener class instead of plugin.

