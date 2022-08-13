---
title: "Hiding Players"
slug: "hiding-players"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
 - void hide(Player toHide);
 - void show(Player toShow);
 - boolean canSee(Player toBeSeen);

Events are better covered in [StackOverflow's List of Events documentation][1]


  [1]: https://www.wikiod.com/bukkit/entity-events

## Hiding a Player from other Players
    Player playerToHide;
    Player playerToNotSee;


    playerToNotSee.hide(playerToHide);
    //playerToHide will no longer be seen by playerToNotSee.

If player is already hidden, nothing happens

## Showing a Player to another Player
    Player toUnhide;
    Player toSeeAgain

    toSeeAgain.show(toUnhide);
    //Player toSeeAgain will now see Player toUnhide again.

If player is already visible, nothing happens.

## Checking if player can be seen
    Player playerToCheck;
    Player playerSeeing;

    boolean isVisible = playerSeeing.canSee(playerToCheck);
    //isVisible returns true if playerSeeing can see playerToCheck and false otherwhise

## Hiding Player from an Entity
This can be done by using the event EntityTargetEvent

Entities won't target the player if you cancel the event: 

    @EventHandler
    public void onEntityTarget(EntityTargetEvent e) {
        Entity target = e.getEntity();
        if(target instanceof Player) {
            Player playerTargetted = (Player) target;
            if (shouldBeInvisible(playerTargetted) {
                e.setCancelled(true);    //Cancel the target event
            }
        }
    }

