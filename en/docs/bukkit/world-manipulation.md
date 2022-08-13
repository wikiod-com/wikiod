---
title: "World Manipulation"
slug: "world-manipulation"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Refer to [World Generation][1] for world generation topics


  [1]: https://www.wikiod.com/bukkit/world-generation

## Creating Explosions
To create an explosion, the following method signatures may be used:

    boolean createExplosion(double x, double y, double z, float power);
    boolean createExplosion(double x, double y, double z, float power, boolean setFire);
    boolean createExplosion(double x, double y, double z, float power,
                            boolean setFire, boolean breakBlocks);
    boolean createExplosion(Location loc, float power);
    boolean createExplosion(Location loc, float power, boolean setFire);

 - x, y, z and loc represent the location where you want the explosion to happen.
 - power represents the power of your explosion, TnT power is 4F.
 - setFire represents the capability of the explosion to set blocks on fire
 - breakBlocks represents the capability of the explosion to destroy blocks around it.
 - all the methods return true if the explosion happenned, and return false if a plugin cancelled the explosion event.


----------

Simulating a TnT explosion that break blocks and set fire at x=0, y=0 and z=0

    createExplosion(0.0, 0.0, 0.0, 4F, true, true);



## Dropping an Item
The following methods can be used to drop an Item somewhere in the world:

    Item dropItem(Location loc, ItemStack is);
    Item dropItemNaturally(Location loc, ItemStack is);

`dropItem` means dropping an Item exactly at the location, returning an Item object.

`dropItemNaturally` means dropping the Item at the location, but with a random offset, meaning it won't be exactly at the location, but very close nearby. This is made to simulate an item being dropped by an entity or a block such as a Dispenser. 

## Generating a Tree
The following methods can be used to generate a tree naturally (as if it was grown from a sapling) into the world.

    boolean generateTree(Location loc, TreeType type);
    boolean generateTree(Location loc, TreeType type, BlockChangeDelegate delegate);

 - Location is where you want the tree to spawn
 - TreeType is the type of the tree you want to spawn, and can be one of the following

**TreeType enum**

| Type| Description|
| ------ | ------ |
| ACACIA | Acacia tree   |
| BIG_TREE | Regular tree, extra tall with branches |
| BIRCH | Birch tree |
| BROWN_MUSHROOM | Big brown mushroom; tall and umbrella-like |
| CHORUS_PLANT | Large plant native to The End |
| COCOA_TREE | Jungle tree with cocoa plants; 1 block wide | 
| DARK_OAK | Dark Oak tree. |
| JUNGLE | Standard jungle tree; 4 blocks wide and tall | 
| JUNGLE_BUSH | Small bush that grows in the jungle |
| MEGA_REDWOOD | Mega redwood tree; 4 blocks wide and tall |
| RED_MUSHROOM | Big red mushroom; short and fat | 
| REDWOOD | Redwood tree, shaped like a pine tree | 
| SMALL_JUNGLE | Smaller jungle tree; 1 block wide |
| SWAMP | Swamp tree (regular with vines on the side) |
| TALL_BIRCH | Tall birch tree | 
| TALL_REDWOD | Tall redwood tree with just a few leaves at the top | 
| TREE | Regular tree, no branches |

 - delegate may be used if you want a class to call for each block changed as a result of this method

Both signatures will return true if the tree was sucessfully generated, false otherwise.

## Spawning Rules
There are some spawning rules in Worlds in Bukkit. They are:

 - Animal Spawning
 - Creature Spawning
 - Amount of the above that can be spawned


----------


----------
**Animal Spawning**

----------
Animal spawning can be split into the following categories:

 - Water Animals
 - Land Animals

To get the amount of animals that can be spawned inside the World at runtime, you can use the method

    int getAnimalSpawnLimit()
For land animals and

    int getWaterAnimalSpawnLimit();
For water animals.

Both limits can be set with the methods

    void setAnimalSpawnLimit(int limit);
    void setWaterAnimalSpawnLimit(int limit);

**Note:** If set to numbers below 0, world's default amount will be used instead.

Minecraft makes an attempt to spawn animals every 400 ticks (default). That can be changed if you desire, using the following signatures:

    void setTicksPerAnimalSpawns(int ticks);
    void setTicksPerWaterAnimalSpawns(int ticks);
 - A value of 1 will mean the server will attempt to spawn animals in this world every tick.
 - A value of 400 will mean the server will attempt to spawn animals in this world every 400th tick.
 - A value below 0 will be reset back to Minecraft's default.

**Note**: If set to 0, animal spawning will be disabled for this world. It's recommended to use setSpawnFlags(boolean, boolean) to control this instead.



