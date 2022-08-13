---
title: "Falling"
slug: "falling"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

There currently isn't any consistent way to avoid an entity gravity suffering, even if you cancel it's movement, the client-side of the player would still try to fall before the event is cancelled.

## Entity Falling Distance
Entity falling distance is the distance the entity have fallen without reaching a block.

It can be used to calculate different damage from falling, or activating an effect after a big fall.


----------


**Retrieving the falling distance**

    float distanceFell = entity.getFallingDistance();

**Setting the falling distance**

This can be used to simulate a different falling distance than the real one. Bukkit will calculate the damage using the new falling distance.

    entity.setFallingDistance(float distance);

## Cancelling Damage
You can cancel a fall damage by using the `EntityDamageEvent`

    @EventHandler
    public void onEntityDamage(EntityDamageEvent e) {
    Entity tookDamage = e.getEntity();

    DamageCause cause = e.getCause();

    if (cause == DamageCause.FALL){
       //Damage was caused by falling, cancel it
       e.setCancelled(true);
    }

