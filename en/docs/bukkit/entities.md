---
title: "Entities"
slug: "entities"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Nearby Entities
To retrieve a list of nearby entities of an entity, one can use

    List<Entity> nearby = entity.getNearbyEntities(double x, double y, double z);

Bukkit will then calculate a bounding box centered around entity, having as parameters:

 - x: 1/2 the size of the box along x axis
 - y: 1/2 the size of the box along y axis
 - z: 1/2 the size of the box along z axis

The list may be empty, meaning that there are no nearby entities with the parameters.

This approach can be used to detect entities near custom projectiles, for example launching an Itemstack and detecting when it collides with a player

## Teleporting an entity to another entity


## Teleporting an entity to a location


## EntityType
The EntityType enum represents all the entities from Bukkit/Spigot.

All its values can be found below.

| Value | Description|
| ------ | ------ |
| AREA_EFFECT_CLOUD | N/A|
| ARMOR_STAND | Mechanical entity with an inventory for placing weapons / armor into. |
| ARROW | An arrow projectile; may get stuck in the ground. |
| BAT | N/A |
| BLAZE | N/A |
| BOAT | A place boat |
| CAVE_SPIDER  | N/A|
|CHICKEN | N/A |
|COMPLEX_PART  | N/A
|COW |N/A|
|CREEPER  | N/A
|DRAGON_FIREBALL| Like FIREBALL, but with extra effects |
| DROPPED_ITEM | An item resting on the ground.
| EGG | A flying chicken egg.
| ENDER_CRYSTAL | N/A
| ENDER_DRAGON  | N/A
| ENDER_PEARL | A flying ender pearl.
| ENDER_SIGNAL | An ender eye signal.
| ENDERMAN | N/A
| ENDERMITE | N/A
| EXPERIENCE_ORB |An experience orb.
| FALLING_BLOCK | A block that is going to or is about to fall.
| FIREBALL | A flying large fireball, as thrown by a Ghast for example.
| FIREWORK | Internal representation of a Firework once it has been launched.
| FISHING_HOOK | A fishing line and bobber.
| GHAST | N/A
|GIANT  | N/A
|GUARDIAN | N/A
|HORSE  | N/A
|IRON_GOLEM  |N/A
| ITEM_FRAME | An item frame on a wall.
| LEASH_HITCH | A leash attached to a fencepost.
| LIGHTNING | A bolt of lightning.
| LINGERING_POTION | A flying lingering potion
| MAGMA_CUBE | N/A
|MINECART |N/A
|MINECART_CHEST|N/A 
|MINECART_COMMAND  | N/A
|MINECART_FURNACE |N/A
|MINECART_HOPPER |N/A
|MINECART_MOB_SPAWNER|N/A 
|MINECART_TNT |N/A
|MUSHROOM_COW |N/A
|OCELOT |N/A
|PAINTING |A painting on a wall.
|PIG |N/A
|PIG_ZOMBIE | N/A 
|PLAYER |N/A
|POLAR_BEAR  | N/A
|PRIMED_TNT|Primed TNT that is about to explode.
|RABBIT  | N/A
|SHEEP |N/A
|SHULKER |N/A
|SHULKER_BULLET|Bullet fired by SHULKER.
|SILVERFISH |N/A
|SKELETON |N/A
|SLIME |N/A
|SMALL_FIREBALL|A flying small fireball, such as thrown by a Blaze or player.
|SNOWBALL|A flying snowball.
|SNOWMAN |N/A
|SPECTRAL_ARROW|Like TIPPED_ARROW but causes the PotionEffectType.GLOWING effect on all team members.
|SPIDER |N/A
|SPLASH_POTION|A flying splash potion
|SQUID |N/A
|THROWN_EXP_BOTTLE|A flying experience bottle.
|TIPPED_ARROW|Like ARROW but tipped with a specific potion which is applied on contact.
|UNKNOWN|An unknown entity without an Entity Class
|VILLAGER |N/A
|WEATHER |N/A
|WITCH |N/A
|WITHER |N/A
|WITHER_SKULL|A flying wither skull projectile.
|WOLF |N/A
|ZOMBIE |N/A

## Passenger
Entities can have passengers. A good example of a passenger is a Player riding a saddled pig, or a zombie inside a minecart.

Although there are specific vehicles, any entity can be a vehicle for any other entity with the SetPassenger method.

    Entity vehicle;
    Entity passenger;
    boolean result = vehicle.setPassenger(passenger);   //False if couldn't be done for whatever reason

The passenger should now be attached to the vehicle


----------


You can check if an entity has a passenger using

    boolean hasPassenger =  entity.isEmpty()


----------


If the entity has a passenger, you can retrieve the passenger entity with

    Entity passenger = entity.getPassenger();
Will only return the primary passenger if the vehicle can have multiples.


----------

Finally, you can eject an entity's passenger with

    boolean b = entity.eject();   //Eject all passengers - returns true if there was a passenger to be ejected

