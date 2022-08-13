---
title: "Spawn Eggs"
slug: "spawn-eggs"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Refer to [Entities Documentation][1] to understand EntityType better


  [1]: https://www.wikiod.com/bukkit/entities#EntityType

## Creating an ItemStack of a SpawnEgg
**For anything below 1.9**
<!-- language-all: lang-java -->
    SpawnEgg egg = new SpawnEgg(EntityType.CREEPER);
    ItemStack creeperEgg = egg.toItemStack(5);

**For 1.9 and above**

In versions 1.9 and higher, Spigot does not have an implementation for creating spawn eggs without using NMS. To accomplish this, you can use a small custom class/wrapper to make it happen:

    public ItemStack toItemStack(int amount, EntityType type) {
        ItemStack item = new ItemStack(Material.MONSTER_EGG, amount);
        net.minecraft.server.v1_9_R1.ItemStack stack = CraftItemStack.asNMSCopy(item);
        NBTTagCompound tagCompound = stack.getTag();
        if(tagCompound == null){
            tagCompound = new NBTTagCompound();
        }
        NBTTagCompound id = new NBTTagCompound();
        id.setString("id", type.getName());
        tagCompound.set("EntityTag", id);
        stack.setTag(tagCompound);
        return CraftItemStack.asBukkitCopy(stack);
    }

