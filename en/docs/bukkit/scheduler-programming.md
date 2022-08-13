---
title: "Scheduler Programming"
slug: "scheduler-programming"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
 -  `Bukkit.getScheduler().scheduleSyncRepeatingTask(Plugin plugin, Runnable task, int initialDelay, int repeatingDelay)`
 -  `Bukkit.getScheduler().scheduleSyncDelayedTask(Plugin plugin, Runnable task, int initialDelay)`
 -  `Bukkit.getScheduler().runTaskAsynchronously(Plugin plugin, Runnable task)`
 -  `Bukkit.getScheduler().runTask(Plugin plugin, Runnable task)`
 -  `new BukkitRunnable() { @Override public void run() { /* CODE */ } }.runTaskLater(Plugin plugin, long delay);`
 -  `new BukkitRunnable() { @Override public void run() { /* CODE */ } }.runTaskTimer(Plugin plugin, long initialDelay, long repeatingDelay);`

Few Bukkit API methods are thread-safe and can be called asynchronously. For this reason, Bukkit API methods should *only*, with a few exceptions, be run on the main thread.

Code run inside of `scheduleSync` methods, as well as the `runTask` method will be run on the main thread.

Code run inside of `runTaskAsynchronously` will be run asynchronously from the main thread. Asynchronous methods are very useful for doing large math or database operations without lagging the server, yet will cause undefined behavior if used to call Bukkit API methods. For this reason, Bukkit API methods that should be run after the asynchronous code should always be put in a `runTask` method.

## Scheduler Repeating task
<!-- language-all: java -->

The time for Scheduler Tasks are measured in Ticks. Under normal conditions, there are 20 ticks per second.

Tasks scheduled with `.scheduleSyncRepeatingTask` will be run on the Main Thread

<pre><code>Bukkit.getScheduler().<b>scheduleSyncRepeatingTask</b>(plugin, new Runnable() {
    @Override
    public void run() {
        Bukkit.broadcastMessage("This message is shown immediately and then repeated every second");
    }
}, 0L, 20L); //0 Tick initial delay, 20 Tick (1 Second) between repeats</code></pre>



## Scheduler Delayed Task
<!-- language-all: java -->

The time for Scheduler Tasks are measured in Ticks. Under normal conditions, there are 20 ticks per second.

Tasks scheduled with `.scheduleSyncDelayedTask` will be run on the Main Thread

<pre><code>Bukkit.getScheduler().<b>scheduleSyncDelayedTask</b>(plugin, new Runnable() {
    @Override
    public void run() {
        Bukkit.broadcastMessage("This message is shown after one second");
    }
}, 20L); //20 Tick (1 Second) delay before run() is called</code></pre>


## Running Tasks Asynchronously
You can make code run asynchronously from the main thread using `runTaskAsynchronously`. This is useful for doing intensive math or database operations, as they will prevent the main thread from freezing (and the server from lagging).

Few Bukkit API methods are thread-safe, so many will cause undefined behavior if called asynchronously from the main thread.

<pre><code>Bukkit.getScheduler().<b>runTaskAsynchronously</b>(plugin, new Runnable() {
    @Override
    public void run() {
        Bukkit.getLogger().info("This message was printed to the console asynchronously");
        //Bukkit.broadcastMessage is not thread-safe
    }
});</code></pre>

## Running Tasks on the Main Thread
You can also make code run synchronously with the main thread using `runTask`. This is useful when you want to call Bukkit API methods after running code asynchronously from the main thread.

Code called inside of this Runnable will be executed on the main thread, making it safe to call Bukkit API methods.

<pre><code>Bukkit.getScheduler().<b>runTask</b>(plugin, new Runnable() {
    @Override
    public void run() {
        Bukkit.broadcastMessage("This message is displayed to the server on the main thread");
        //Bukkit.broadcastMessage is thread-safe
    }
});</code></pre>

## Running a BukkitRunnable
The BukkitRunnable is a Runnable found in Bukkit. It's possible to schedule a task directly from a BukkitRunnable, and also cancel it from inside itself.

Important: The time on the tasks is measured in Ticks. A second has 20 ticks.

    

**Non-RepeatingTask:**

    JavaPlugin plugin;    //Your plugin instance    
    Long timeInSeconds = 10;
    Long timeInTicks = 20 * timeInSeconds;
    new BukkitRunnable() {
            
        @Override
        public void run() {
            //The code inside will be executed in {timeInTicks} ticks.
            
        }
    }.runTaskLater(plugin, timeInTicks);   // Your plugin instance, the time to be delayed.

**Repeating Task:**

    JavaPlugin plugin;    //Your plugin instance    
    Long timeInSeconds = 10;
    Long timeInTicks = 20 * timeInSeconds;
    new BukkitRunnable() {
            
        @Override
        public void run() {
            //The code inside will be executed in {timeInTicks} ticks.
           //After that, it'll be re-executed every {timeInTicks} ticks;
          //Task can also cancel itself from running, if you want to.

           if (boolean) {
               this.cancel();
           }
            
        }
    }.runTaskTimer(plugin, timeInTicks, timeInTicks);   //Your plugin instance, 
                                                       //the time to wait until first execution,
                                                      //the time inbetween executions.



## Running Thread Safe Code from an Asynchronous Task
Sometimes you'll need to execute synchronous code from within an asynchronous task. To do this, simply schedule a synchronous task from within the asynchronous block.

    Bukkit.getScheduler().runTaskTimerAsynchronously(VoidFlame.getPlugin(), () -> {

        Bukkit.getScheduler().runTask(VoidFlame.getPlugin(), () -> {
            World world = Bukkit.getWorld("world");
            world.spawnEntity(new Location(world, 0, 100, 0), EntityType.PRIMED_TNT);
        });

    }, 0L, 20L);

