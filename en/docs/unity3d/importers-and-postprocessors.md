---
title: "Importers and (Post)Processors"
slug: "importers-and-postprocessors"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Syntax
- AssetPostprocessor.OnPreprocessTexture()

Use `String.Contains()` to process only assets that have a given string in their asset paths.

<!-- language: c# -->
    if (assetPath.Contains("ProcessThisFolder"))
    {
        // Process asset
    }

## Texture postprocessor
Create `TexturePostProcessor.cs` file anywhere in **Assets** folder:


<!-- language: c# -->


    using UnityEngine;
    using UnityEditor;
    
    public class TexturePostProcessor : AssetPostprocessor
    {
        void OnPostprocessTexture(Texture2D texture)
        {
            TextureImporter importer = assetImporter as TextureImporter;
            importer.anisoLevel = 1;
            importer.filterMode = FilterMode.Bilinear;
            importer.mipmapEnabled = true;
            importer.npotScale = TextureImporterNPOTScale.ToLarger;
            importer.textureType = TextureImporterType.Advanced;
        }
    }

Now, every time Unity imports a texture it will have the following parameters:
[![enter image description here][1]][1]

> If you use postprocessor, you can not change texture parameters by manipulating **Import Settings** in editor.  
When you hit **Apply** button the texture will be reimported and postprocessor code will run again.

  [1]: http://i.stack.imgur.com/gkhQK.png

## A Basic Importer


