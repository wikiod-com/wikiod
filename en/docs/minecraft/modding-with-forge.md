---
title: "Modding with Forge"
slug: "modding-with-forge"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - MODID         = represents the Identifier of the MOD
 - MODPath = stands for the full qualified directory path to your mod folder



This topic should contain most used patterns/examples and well tested code for modding the Minecraft application with forge. 
Maybe this can replace the official documentation one day.

## Implementation pattern for Initialization Proxies
This example shows you how to implement proxy classes for your Minecraft Mod Application, which are used to initialize your mod.

First of all you will need to implement the base CommonProxy.java class which contains the 3 mainly used method:

    public class CommonProxy {
        public void preInit(FMLPreInitializationEvent e) {}
        public void init(FMLInitializationEvent e) {}
        public void postInit(FMLPostInitializationEvent e) {}
    }

Normally your mod has 2 different packages for Client and Server Code, so you will need in each package a child class of CommonProxy.java like:

    public class ClientProxy extends CommonProxy {    
        @Override
        public void preInit(FMLPreInitializationEvent e) {
            super.preInit(e);
        }
    
        @Override
        public void init(FMLInitializationEvent e) {
            super.init(e);    
        }
    
        @Override
        public void postInit(FMLPostInitializationEvent e) {
            super.postInit(e);
        }
    }

and for the server:

    public class ServerProxy extends CommonProxy {
        @Override
        public void preInit(FMLPreInitializationEvent e) {
            super.preInit(e);
        }
    
        @Override
        public void init(FMLInitializationEvent e) {
            super.init(e);
        }
    
        @Override
        public void postInit(FMLPostInitializationEvent e) {
            super.postInit(e);
        }
    }

After you have created this classes you are able to extend them by methods which have to run only on client or server side, but can also attach them to both if you call the methods in the 'base' class.

Lastly you have to define which proxy is taken at runtime. You have to extend your main mod class with the `@Mod` annotation, by:
    
    private static final String CLIENTPROXY = "com.yourpackage.client.ClientProxy";
    private static final String SERVERPROXY = "com.yourpackage.client.ServerProxy";    

    @SidedProxy(clientSide = CLIENTPROXY, serverSide = SERVERPROXY)
    public static CommonProxy PROXY;

This will enable Forge to detect which class should be taken at runtime. In the initialization methods of your Mod you can now use this static PROXY property.

    @EventHandler
    public void init(FMLInitializationEvent event) {
        PROXY.init(event);
    }

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        PROXY.preInit(event);
    }

    @EventHandler
    public void postInit(FMLPostInitializationEvent event) {
        PROXY.postInit(event);
    }

## Adding custom sounds to your MOD
This example show you how you add new sounds to your MOD and play them. First of all you need a sound file which has the format `*.ogg`. Any other format is not allowed by the Minecraft application and will be rejected.

The soundfile has the name: sound1.ogg

Put the sound file under the following path:

> /YourPath/src/main/resources/assets/MODID/sounds/sound1.ogg

Replace 'MODID' by the identifier you defined for your MOD

Next you have to create a `sounds.json` in UTF-8 (Standard) encoding which defines name, resource,... and other things for your custom sound. This file will look like:

    {
      "sound1": {
        "category" : "player",
        "sounds": [{
            "name": "MODID:sound1",
            "stream": false
          }
        ]
      },
      "sound2": {
        "category" : "ambient",
        "sounds": [{
            "name": "MODID:subfolder/sound2",
            "stream": true
          }
        ]
      }
    }

As explanation for this sounds.json.

There are defined 2 sounds defined, as I added an example that you can investigate how to add multiply sounds. `sound1` has the category `player` the second is of category `ambient` which means the volume of the sound is affected by the volume settings the user has set for player/ambient sounds. `name` is the most important property as it is pointing to the resource of the sound. The `MODID` is the identifier of your MOD and is mandatory as the Application will search in the resources of your mod for the file, otherwise it will search in the Minecraft resources and will find nothing. The `stream` property means that the sound will be streamed from file system, which is only needed for sounds longer than 4 seconds.

Your custom `sounds.json` file has to go under the following path:

> /YourPath/src/main/resources/assets/MODID/sounds.json

Now you will be able to load the sounds to the registry of the game. So you have to create a class which is initializing `SoundEvent`s and handling the registration.

    public class SoundRegistrator {
        public static final SoundEvent SOUND_1;
        public static final SoundEvent SOUND_2;
    
        static {
            SOUND_1 = addSoundsToRegistry("sound1");
            SOUND_2 = addSoundsToRegistry("sound2");
        }
    
        private static SoundEvent addSoundsToRegistry(String soundId) {
            ResourceLocation shotSoundLocation = new ResourceLocation("MODID", soundId);
            SoundEvent soundEvent = new SoundEvent(shotSoundLocation);
            soundEvent.setRegistryName(shotSoundLocation);
            return soundEvent;
        }           
    }
    
Afterward you have to create a `SoundRegisterListener`:
    
    public class SoundRegisterListener {
            @SubscribeEvent(priority = EventPriority.NORMAL, receiveCanceled = true)
            public void registerSoundEvents(RegistryEvent.Register<SoundEvent> event) {
               event.getRegistry().registerAll(SoundRegistrator.SOUND_1,SoundRegistrator.SOUND_2);
            }   
    }

and register it to the `MinecraftForge.EVENT_BUS` like:

    MinecraftForge.EVENT_BUS.register(new SoundRegisterListener());

Finally you will be able to play your sounds:

    void playSound(SoundEvent sound) {
        try {
            if (Minecraft.getMinecraft().world.isRemote) {
                EntityPlayerSP player = Minecraft.getMinecraft().player;
                Minecraft.getMinecraft().world.playSound(player, player.getPosition(), sound, SoundCategory.PLAYERS, RandomGenerator.getNextRandomVolumeLoud(), 1.0F);
            }
        } catch (Exception ex) {
           //Error happened
        }
    }

## Sending a command
This example shows you different ways to execute 'commands' for Minecraft from code:

    EntityPlayerSP player = Minecraft.getMinecraft().player;
    player.sendChatMessage("/Command here");

to send a command in SinglePlayer

