---
title: "World generation"
slug: "world-generation"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Void Generator
The void generator class:

    public class VoidGenerator extends ChunkGenerator
    {
    
        @SuppressWarnings("deprecation")
        public byte[] generate(World w, Random rand, int x, int z)
        {
            byte[] result = new byte[32768]; //chunksized array filled with 0 - Air
            //Build a platform with Bedrock where the player shall spawn later
            if(x == 0 && z == 0)
            {
                result[xyz(0, 64, 0)] = (byte)Material.BEDROCK.getId();
                result[xyz(1, 64, 0)] = (byte)Material.BEDROCK.getId();
                result[xyz(0, 64, 1)] = (byte)Material.BEDROCK.getId();
            }
            return result;
        }
    
        private Integer xyz(int x, int y, int z)
        {
            return (x * 16 + z)*128+y; //position inside the chunk
        }
        
    }


Any class where you want to generate a new World:

    public void generateWorld(String mapName)
    {
        try
        {
            WorldCreator w = new WorldCreator(mapName);
            w.generateStructures(false); //no trees, etc.
            w.generator(new VoidGenerator()); //use the VoidGenerator
            w.environment(Environment.NORMAL); //no nether, etc.
            w.createWorld(); //create the world
        } 
        catch(Exception ex) 
        { 
          ex.printStackTrace(); 
        }
    }



