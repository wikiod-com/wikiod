---
title: "blend-model (component)"
slug: "blend-model-component"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

blend-model component Loads a three.js format JSON model containing skeletal animation blending using **THREE.BlendCharacter**. This is mainly used to represent the hand and Vive controllers.

## Syntax
- `<a-entity blend-model="#a-asset-item-selector"></a-entity>`

# VALUES

| Type     | Description                             |
|----------|-----------------------------------------|
| **selector** | Selector to an `<a-asset-item>`         |
| **string**   | `url()`-enclosed path to a JSON file    |

# EVENTS

| Event Name | Description |
| --- | --- |
| **model-loaded** | JSON model was loaded into the scene. |

## Example usage of `blend-model`
We can load the model by pointing using the ID to an <a-asset-item> that specifies the src to a file:

<!-- language: lang-html --->

    <a-scene>
      <a-assets>
        <!-- At first we load skeletal animation blending JSON as asset -->
        <a-asset-item id="hand" src="/path/to/hand.json"></a-asset-item>
      </a-assets>
      <!-- Now we can use that asset with blend-model-->
      <a-entity blend-model="#hand"></a-entity>
    </a-scene>

