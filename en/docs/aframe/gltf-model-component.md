---
title: "gltf-model (component)"
slug: "gltf-model-component"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

The gltf-model component allows to use 3D models in the glTF format with A-Frame.
glTF is a Khronos standard for efficient, full-scene 3D models and is optimised for usage on the web.

## Syntax
- `<a-entity gltf-model="url(https://cdn.rawgit.com/KhronosGroup/glTF-Sample-Models/9176d098/1.0/Duck/glTF/Duck.gltf)"></a-entity>`
- `<a-entity gltf-model="#duck"></a-entity>`

## Parameters
| Parameter | Details |
| --- | --- |
| `url(...)` | will load the glTF model wrapped from the URL wrapped in `url()`
| `#example` | will load the `<a-asset-item>` with the ID `example`

## Loading a glTF model via URL
    <a-scene>
      <a-entity gltf-model="url(https://cdn.rawgit.com/KhronosGroup/glTF-Sample-Models/9176d098/1.0/Duck/glTF/Duck.gltf)" position="0 0 -5"></a-entity>
    </a-scene>



## Loading a gltf-model via the asset system
    <a-scene>
      <a-assets>
        <a-asset-item id="duck" src="https://cdn.rawgit.com/KhronosGroup/glTF-Sample-Models/9176d098/1.0/Duck/glTF/Duck.gltf"></a-asset-item>
      </a-assets>

      <a-entity gltf-model="#duck" position="0 0 -5"></a-entity>
    </a-scene>

