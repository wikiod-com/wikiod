---
title: "Player Events"
slug: "player-events"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

This is a List of Player Events and an example on how to use them.

## PlayerJoinEvent
<!-- language-all: java -->
    public class PlayerJoinListener implements Listener {
        @EventHandler
        public void onPlayerJoin(PlayerJoinEvent evt) {
            Player joined = evt.getPlayer();
            String joinedName = joined.getName();

           //RETRIEVING THE JOIN MESSAGE ALREADY SET
           String joinMessage = evt.getJoinMessage();

           //SETTING THE JOIN MESSAGE
           evt.setJoinMessage(joinedName + " has joined the game");

           //CLEARING THE JOIN MESSAGE
           evt.setJoinMessage(null);
        }
    }

## PlayerMoveListener
<!-- language-all: java -->

    public class PlayerMoveListener implements Listener {
        @EventHandler
        public void onPlayerMove(PlayerMoveEvent evt) {
            Location from = evt.getFrom();
            Location to = evt.getTo();
            double xFrom = from.getX();
            double yFrom = from.getY();
            double zFrom = from.getZ();
            double xTo = to.getX();
            double yTo = to.getY();
            double zTo = to.getZ();

            Bukkit.getLogger().info("Player " + evt.getPlayer().getName() 
                            +  " has moved from x: " + xFrom + " y: " + yFrom + " z: " 
                            + zFrom + " to x: " + xTo + " y: " + yTo + " z: " + zTo);
        }
    }

## PlayerLoginEvent
Event stores details for players attempting to log in

    @EventHandler
    public void onPlayerLogin(PlayerLoginEvent e) {
        Player tryingToLogin = e.getPlayer();

        //Disallowing a player login
        e.disallow(PlayerLoginEvent.Result.KICK_FULL , "The server is reserved and is full for you!");

        //Allowing a player login
        if (e.getResult() != PlayerLoginEvent.Result.ALLOW) {
            if (isVip(tryingToLogin) ){
                e.allow();
            }
        }

        //Getting player IP
        String ip = e.getAddress();

        //Get the hostname player used to login to the server
        String ipJoined = e.getHostname();

        //Get current result from the login attempt
        PlayerLoginEvent.Result result = e.getResult();

        //Set kick message if Result wasn't ALLOW
        e.setKickMessage("You were kicked!");

        //Retrieve the kick message
        String s = e.getKickMessage();

    }


----------


PlayerLoginEvent.Result ENUM:

> - ALLOWED - The player is allowed to log in
> - KICK_BANNED - The player is not allowed to log in, due to them being banned
> - KICK_FULL - The player is not allowed to log in, due to the server being full
> - KICK_OTHER - The player is not allowed to log in, for reasons undefined
> - KICK_WHITELIST - The player is not allowed to log in, due to them not being on the white list

## Player Bed Events
Event fired when player enters a bed:
**PlayerBedEnterEvent**
> PlayerBedEnterEvent(Player who, Block bed)

    @EventHandler
    public void onPlayerBedEnter(PlayerBedEnterEvent e) {
        Player entered = e.getPlayer();

       Block bedEntered = e.getBed();
    }


----------


----------


Event fired when player leaves a bed: **PlayerBedLeaveEvent**



> PlayerBedLeaveEvent(Player who, Block bed) 

    @EventHandler
    public void onPlayerBedEnter(PlayerBedEnterEvent e) {
        Player entered = e.getPlayer();

       Block bedEntered = e.getBed();
    }



