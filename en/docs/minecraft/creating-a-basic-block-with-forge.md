---
title: "Creating a basic block with Forge"
slug: "creating-a-basic-block-with-forge"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Creating a simple, decorative block with Forge is one of the first tasks an aspiring modder will have to learn.  How to do that has changed over the various versions of Minecraft and is probably at a "moderate" difficulty post 1.7.10 due to the sheer number of easy to make mistakes.

If something goes wrong and your custom block (either when placed or held) has a missing texture (black/purple) or model (default cube that's too large when held) check the log.  Problems of this sort will almost *always* show up in the log, provided that you have registered things correctly.

The first thing that will show up is `"Exception loading model for variant..."` line or similar, telling you which block or item failed to load properly.  After a dozen or so lines beginning with `    at...` will be another line starting `Caused by...`

This "Caused By" line is the important one, it will tell you which file failed to load properly and can be one of several errors, such as:

 - Malformed JSON (your JSON file isn't valid and has a typo)
 - File Not Found (the file Minecraft is looking for is not properly named or in the right place)
 - Missing Variant Exception (your Blockstate JSON is incomplete)

You may even get more than one failing error for a single mistake!  If you don't kno which block is the problem, comment out every block and item you know works, reduce the error list down to the block (or blocks) in question.  You may have to do this several times.

Additionally, a mixing texture shows up as differently as a simple list all of the missing resources for a given domain (mod ID).

## The Block Class
First we need a class that represents the block

    public class CustomBlock extends Block {
    
        public CustomBlock () {
            super(Material.ROCK);
            setHardness(1.0f);
            setHarvestLevel("pickaxe", 0);
            setResistance(1.0f);
            setCreativeTab(CreativeTabs.DECORATIONS);
            this.setSoundType(SoundType.STONE);
        }
    }

Even here there are several modifications available: material (which governs some properties such as being able to be pushed by pistons and whether it can be broken by hand), hardness (how long it takes to break), harvest level (appropriate tool and tool material: in this case wooden pickaxe), resistance (vs. explosions), the tab it shows up in the creative menu, and what step sound it has.

This is where any fancy functionality blocks will have will need to go, but for now we're making a block that just looks nice, so we're done.

## The Block Model JSON
Next we need to tell Minecraft what we want our block to look like.

    {
        "parent": "block/cube_all",
        "textures": {
            "all": "example:blocks/decorative"
        }
    }

That's pretty much all that's needed for it to work once the block is registered.  The only important thing is that the **filename** match the **registry name** used to register the block and should be in all lowercase (1.11+ file names are *required* to be lowercase, prior to that it is just case sensitive).

Name the model JSON file `my_block.json` (matching the registry name we're going to give it later) and save it at `src\main\resources\assets\example\models\block\` (where `example` is the mod ID specified in the @Mod annotation of your main mod class).

The block model here uses a parent of block/cube_all, which means that the single supplied texture will be used on all faces.  There are other default models as well, such as:

 - block/cube (all six faces assigned independently)
 - block/cube_bottom_top (top and bottom faces independent from the sides)
 - block/orientable (directional facing block, eg. furnace)
 - block/cross (flowers, tall grass)
 - block/crop (wheat, carrots)

Do note that each model specifies the textures it uses by a name ID (e.g. `"all"` or `"top"`). Look at the parent model to determine what those names are if you are uncertain. Incorrectly specified textures may lead to *non-error-reporting* missing texture issues.

It is also possible to create a wholly custom model or to create a custom parent model. But for now, this will be enough.

Don't forget to create a texture, name it `decorative.png` (as that's what the JSON file specified) and save it to `src\main\resources\assets\example\textures\blocks\`

## Block Registration
Registering blocks is done from your main mod class, or a ModBlocks class method invoked from the main mod class during preInit.

    Block myBlock = new CustomBlock();
    string registryname = "my_block";
    block.setRegistryName(registryname);
    block.setUnlocalizedName(block.getRegistryName().toString());
    GameRegistry.register(block);

There is an important reason to use `block.setUnlocalizedName(block.getRegistryName().toString());` as well! It insures that your block (and item) unlocalized names contain the mod ID to avoid language file conflicts between mods.

Oh you want an item version too so it can exist in your inventory too?  That's created separately post 1.7.10 and done like so:

    ItemBlock ib = new ItemBlock(block);
    ib.setRegistryName(registryname);
    GameRegistry.register(ib);

Notice that we set the ItemBlock's registry name to the same string as our block. This is how Forge and match blocks to their ItemBlock counterpart and vice versa.

But wait, there's more!

Your block might have an *item form*, but that item doesn't have a model or texture yet! Models are automatically registered for blocks, but not items.  This *may only be called from the Client Proxy* and does not cover blocks with variants (such as wool or leaves).

    ModelLoader.setCustomModelResourceLocation(
        ib , 0, new ModelResourceLocation(ib.getRegistryName(),"normal"));

Generally speaking you will not also need an Item Model JSON as Forge and vanilla will fall back on the block's model instead, however this is not always the case.  If you do find that you need an Item model JSON, just parent it to your block JSON and save it to `src\main\resources\assets\example\models\item\` with the same file name as the block's registry name.

    {
        "parent": "example:block/my_block"
    }

