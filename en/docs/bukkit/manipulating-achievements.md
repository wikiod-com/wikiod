---
title: "Manipulating Achievements"
slug: "manipulating-achievements"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Syntax
 - player.awardAchievement(Achievement ach);

## Awarding Achievements
    Player player;     //The player you want to award your achievement with
    Achievement achievement;     //The achievement you want to award your player

    player.awardAchievement(achievement);    //Awarding the achievement

Checking if player has achievement:

    Player player;
    Achievement achievement;
    boolean hasAchievement = player.hasAchievement(achievement);

The code will award the achievement and any parent achievement.




----------


Possible achievements are:

> - ACQUIRE_IRON (Acquire Hardware)
> - BAKE_CAKE (The Lie)
> - BOOKCASE (Librarian)
> - BREED_COW (Repopulation)
> - BREW_POTION (Local Brewery)
> - BUILD_BETTER_PICKAXE (Getting an Upgrade)
> - BUILD_FURNACE (Hot Topic)
> - BUILD_HOE (Time to Farm!)
> - BUILD_PICKAXE (Time to Mine!)
> - BUILD_SWORD (Time to Strike!)
> - BUILD_WORKBENCH (Benchmarking)
> - COOK_FISH (Delicious Fish)
> - DIAMONDS_TO_YOU (Diamonds to you!)
> - ENCHANTMENTS (Enchanter)
> - END_PORTAL (The End?)
> - EXPLORE_ALL_BIOMES (Adventuring Time)
> - FLY_PIG (When Pigs Fly)
> - FULL_BEACON (Beaconator)
> - GET_BLAZE_ROD (Into Fire)
> - GET_DIAMONDS (DIAMONDS!)
> - GHAST_RETURN (Return to Sender)
> - KILL_COW (Cow Tipper)
> - KILL_ENEMY (Monster Hunter)
> - KILL_WITHER (The Beginning.)
> - MAKE_BREAD  (Bake Bread)
> - MINE_WOOD (Getting Wood)
> - NETHER_PORTAL (We Need to Go Deeper)
> - ON_A_RAIL (On A Rail)
> - OPEN_INVENTORY (Taking Inventory)
> - OVERKILL (Overkill)
> - OVERPOWERED (Overpowered)
> - SNIPE_SKELETON (Sniper Duel)
> - SPAWN_WITHER (The Beginning?)
> - THE_END (The End.)


[Reference][1]


  [1]: http://minecraft.gamepedia.com/Achievements

