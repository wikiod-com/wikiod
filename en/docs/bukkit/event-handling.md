---
title: "Event Handling"
slug: "event-handling"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

When something happens inside Bukkit, an Event is called so every plugin can decide what to do whenever something happens. 

An Event is called when a player tries to play a block, when an entity despawn, when someone logs in... Plugins can listen to specific events and deal with it in many different ways, for example, sending a message to an admin when a Player logs in, via the PlayerLoginEvent.

## Syntax
 - Bukkit.getPluginManager().registerEvents(Listener l, Plugin p);

When registering an event, take a look if you're not registering it twice! Or your plugin will act twice for the registered event.

Take an extra look for how to handle specific events:

 - [Player Events][1]
 - [Entity Events][2]


  [1]: https://www.wikiod.com/bukkit/player-events
  [2]: https://www.wikiod.com/bukkit/entity-events

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

## Basic Event Handling
Bukkit uses an event based system that allows plugin developers to interact with and modify the server and specific actions that occur in the world. 

<br>

# Creating an Event Handler

Event handlers are methods that get called when their event occurs. They are generally public and void as well as named `on{EventNameStem}` by convention. All handlers however, must have the `@EventHandler` annotation, as well as contain its event as the ONLY parameter. Here is an example of an event handler for the `PlayerJoinEvent`

```
@EventHandler
public void onPlayerJoin(PlayerJoinEvent event){
    //Run when a player joins
}
```

**Note:** The naming format for Bukkit events is `{Source}{Action}({Target})Event`. Some examples of these event names are: `PlayerInteractEvent` or `BlockBreakEvent` or `PlayerJoinEvent`. A list of all events can be found on the [Spigot Javadocs](https://hub.spigotmc.org/javadocs/spigot/org/bukkit/event/class-use/Event.html)


<br>

# Registering Events

Merely creating an event handler is not enough to allow Bukkit to start sending event calls to your method. You must also register it through the PluginManager interface.

The most common way to register events is to create a class that implements the Listener interface and use it wrap your event handlers.

```
public class EventListener implements Listener { //Implements the Listener interface

    @EventHandler
    public void onPlayerJoin(PlayerJoinEvent event){
        //Run when a player joins
    }

}
```

This listener class and all of its events can then be registered in your main plugin class like this:

```
@Override
public void onEnable(){
    Bukkit.getPluginManager().registerEvents(new EventListener(), this); //Register your listener and its event handlers
}
```

## Register Events inside the Listener class
    import org.bukkit.event.Listener;
    import org.bukkit.event.EventHandler;
    import org.bukkit.event.EventPriority;
    import org.bukkit.event.player.PlayerLoginEvent;
    import org.bukkit.event.player.PlayerQuitEvent;

    public class MyEventListener implements Listener {

        /**
         * Constructor
         */
        public MyEventListener(Main plugin){
            //register Events of this class
            //with methode: registerEvents(Listener, Plugin);
            plugin.getServer().getPluginManager().registerEvents(this, plugin);
        }      
    
        /**
         * A Event with HIGH priority
         */
        @EventHandler(priority = EventPriority.HIGH) //An EventHandler annotation
        public void onPlayerLogin(PlayerLoginEvent event){  //A bukkit event
            event.getPlayer().sendMessage("Welcome.");
        }
        /**
         * A Event with NORMAL (default) priority
         */
        @EventHandler    
        public void onPlayerQuit(PlayerQuitEvent event){ 
            Bukkit.broadcastMessage(event.getPlayer().getName() + " left the Server.");
        }
            

    }

    /**
     * Main class
     */
    public class Main extends JavaPlugin {
        public void onEnable(){
            //Register Events
            new MyEventListener(this);
        }
    }

## Registering Events to your Main class
    public class Main extends JavaPlugin {

        @Override
        public void onEnable() {
            Bukkit.getPluginManager().registerEvents(this, this);   
        }
      
        @EventHandler
        public void yourEvent(Event e) {
        //...
        }
    }

## Event Priorities
Bukkit has a system called **Event Priorities** to help plugins handle events in the correct older. The seven priorities are (in older from first executed to last):

 - Lowest
 - Low
 - Normal (default)
 - High
 - Highest
 - Monitor

If you are planning to cancel a lot of events (e.g. protection plugin) it would be a good idea to use lower priority (or lowest) to avoid problems.

You should never modify outcome of an event at MONITOR.

<!-- language: lang-java -->

    @EventHandler //same as @EventHandler(priority = EventPriority.NORMAL)
    public void onLogin(PlayerLoginEvent event) {
        // normal login
    }

    @EventHandler(priority = EventPriority.HIGH)
    public void onLogin(PlayerLoginEvent event) {
        // high login
    }


More info:
 - [EventPriority at spigot javadocs][1]
 - [Event Priorities at BukkitWiki][2]


  [1]: https://hub.spigotmc.org/javadocs/spigot/org/bukkit/event/EventPriority.html "EventPriority at spigot javadocs"
  [2]: https://bukkit.gamepedia.com/Event_API_Reference#Event_Priorities "Event Priorities at BukkitWiki"

## Listening to Events


