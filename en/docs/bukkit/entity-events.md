---
title: "Entity Events"
slug: "entity-events"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

All Entity Events extends EntityEvent, the superclass for EntityEvents.

All known EntityEvents can be found below, and will be covered in this documentation.


## EntityEvent (Superclass)
The known sub-classes for Entity Events are:

|Sub-Classes| Sub-Classes | Sub-Classes |
| ------ | ------ |  ------ |
|CreatureSpawnEvent|CreeperPowerEvent|EntityChangeBlockEvent|
|EntityCombustEvent|EntityCreatePortalEvent|EntityDamageEvent|
|EntityDeathEvent|EntityExplodeEvent|EntityInteractEvent|
|EntityPortalEnterEvent|EntityRegainHealthEvent|EntityShootBowEvent|
|EntityTameEvent|EntityTargetEvent|EntityTeleportEvent|
|EntityUnleashEvent|ExplosionPrimeEvent|FoodLevelChangeEvent|
|HorseJumpEvent|ItemDespawnEvent|ItemSpawnEvent|
|PigZapEvent|ProjectileHitEvent|ProjectileLaunchEvent|
|SheepDyeWoolEvent|SheepRegrowWoolEvent|SlimeSplitEvent| 


----------


----------


In addition to this, all the sub-classes inherit the following methods:

    Entity getEntity();            //Entity who is involved in this event
    EntityType getEntityType();    //EntityType of the Entity involved in this event



## EntityDamage Event
The EntityDamage event is thrown when an Entity is damaged.

## EntityDamageEvent ##
    @EventHandler
    public void onEntityDamage(EntityDamageEvent e) {
        DamageCause cause = e.getCause();    //Get the event DamageCause
        double rawDamage = e.getDamage();    //Returns the damage before any calculation
        double damageModified = e.getDamage(DamageModifier.X);     //Returns the damage that would be caused with the specified modifier
        double finalDamage = e.getFinalDamage();    //Gets the final damage of this event, with all the calculations included

        e.setCancelled(boolean x);    //If for any reasons you want the event to not happen, you can cancel it
        e.setDamage(double damage);    //You can change the full damage the event will cause
        e.setDamage(DamageModifier modifier, double damage);    //Changes the damage considering any possible modifier
    }


----------

Most of the times, the EntityDamageEvent will not be used. Instead, one of it's subclasses will be used, such as **EntityDamageByEntityEvent** or **EntityDamageByBlockEvent**. Both can be seen below.

----------

## EntityDamageByEntityEvent ##


    @EventHandler
    public void onEntityDamageByEntity(EntityDamageByEntityEvent e) {
        //Retrieving the Entity that dealt damage
        Entity damageDealer = e.getDamager();

        //Retrieving the Entity that took damage
        Entity damageTaker = e.getEntity();

        //Retrieving the cause of the damage
        DamageCause cause = e.getDamageCause();

        //damage is the double value of the damage before all the resistances and modifiers have been applied
        double damage = e.getDamage();

        //FinalDamage is the double value of the damage after all the resistances and modifiers have been applied
        double finalDamage = e.getFinalDamage();

       //You can also set the raw damage (before modifiers) for the event to a different value
       e.setDamage(20.0);
    }


----------


## EntityDamageByBlockEvent  ##

A simple extension to the EntityDamageEvent, but with one different method:

    Block b = event.getDamager();    //Returns the block that dealt damage to the entity

