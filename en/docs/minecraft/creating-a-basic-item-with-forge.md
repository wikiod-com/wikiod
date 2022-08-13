---
title: "Creating a Basic Item with Forge"
slug: "creating-a-basic-item-with-forge"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Creating a simple item with Forge is one of the first tasks an aspiring modder will have to learn. How to do that has changed over the various versions of Minecraft and is probably at a "moderate" difficulty post 1.7.10 due to the sheer number of easy to make mistakes, particularly with making it render properly.

If something goes wrong and your custom item has a missing texture (black/purple) or model (default cube that's too large when held) check the log.  Problems of this sort will almost *always* show up in the log, provided that you have registered things correctly.

The first thing that will show up is `"Exception loading model for variant..."` line or similar, telling you which block or item failed to load properly.  After a dozen or so lines beginning with `    at...` will be another line starting `Caused by...`

This "Caused By" line is the important one, it will tell you which file failed to load properly and can be one of several errors, such as:

 - Malformed JSON (your JSON file isn't valid and has a typo)
 - File Not Found (the file Minecraft is looking for is not properly named or in the right place)
 - Missing Variant Exception (your Blockstate JSON is incomplete)

You may even get more than one failing error for a single mistake!  If you don't know which item is the problem, comment out every block and item you know works, reduce the error list down to the block (or blocks) in question.  You may have to do this several times.

Additionally, a mixing texture shows up as differently as a simple list all of the missing resources for a given domain (mod ID).

## Item Class
This part hasn't changed over the versions of Minecraft a whole lot, although there have been some mutations in the exact method signatures as well as class hierarchy.  A basic item (one that has no functionality, such as a stick or ingot: that's right, both are do-nothing items!)

    public class CustomItem extends Item {
    
        public CustomItem () {
            super();
            this.setMaxDamage(0);
            this.setCreativeTab(CreativeTabs.MISC);
        }
    }

Not much room for customization at this point, unlike blocks. About all we can do is change whether or not the item can take damage (tools use this) and what creative tab it will exist in. Name and texture we'll handle when we register the item with the GameRegistry.

However, this is all that is needed in order to hold, carry, drop, craft, smelt and otherwise utilize the item.  Some items in Minecraft (such as sticks) don't even have a unique class and simply use `new Item()`. We could do that here, however any item with additional functionality will need a class.

## Item Model
As with blocks, items need models too.

    {
        "parent": "item/generated",
        "textures": {
            "layer0": "example:items/basic"
        }
    }

That's pretty much all that's needed for it to work once the item is registered. The only important thing is that the filename match the registry name used to register the block and should be in all lowercase (1.11+ file names are required to be lowercase, prior to that it is just case sensitive).

Note that "layer0" is the only texture needed and it is highly unlikely that any other texture will be specified at all (although some items like potions and leather armor do have a "layer1"). All names are defined by `item/builtin` (the internal top-most parent model for items) unlike with blocks.

Name the model JSON file `my_item.json` (matching the registry name we're going to give it later) and save it at `src\main\resources\assets\example\models\item\` (where `example` is the mod ID specified in the @Mod annotation of your main mod class).

Additionally create a texture for your item, name it `basic.png` and save it to `src\main\resources\assets\example\textures\items\`

The item model here uses a parent of item/generated, which means that the single supplied texture will be used (as with most non-block items) and will be held in the player's hand in a default orientation.  There is also item/handheld which specifies different display orientations (for tools).  Items may also supply their own "display" attribute, overriding those from the parent, but is not needed in 99.9% of uses.

## Item Registration
Registering items is done from your main mod class, or a ModItems class method invoked from the main mod class during preInit.

    Item item = new CustomItem();
    string registryname = "my_item";
    item.setRegistryName(registryname);
    item.setUnlocalizedName(item.getRegistryName().toString());
    GameRegistry.register(item);

There is an important reason to use `item.setUnlocalizedName(item.getRegistryName().toString());` as well! It insures that your item's unlocalized name contain the mod ID to avoid language file conflicts between mods.

Now the item needs a model too, and this is where things got difficult post 1.7.10, which just involved telling Minecraft the name of the texture to load and could be specified in the item's constructor.

    final ModelResourceLocation fullModelLocation = new ModelResourceLocation(item.getRegistryName().toString(), "inventory");
    ModelBakery.registerItemVariants(item, fullModelLocation);
    ModelLoader.setCustomMeshDefinition(item, new ItemMeshDefinition()
        {
            public ModelResourceLocation getModelLocation(ItemStack stack)
            {
                return fullModelLocation;
            }
        });

Note that this section *must* be located client-side only (i.e. the client proxy) as many of the referenced classes do not exist on the dedicated server.

Registering items with *variants* e.g. Saplings, has to be done a different way, using `ModelLoader.setCustomModelResourceLocation(item, metadata, resourceLocation)` although it lets us use a Blockstate file to specify our variants (which is *much* preferred to the alternative). Our item doesn't use variants, so we're done.

