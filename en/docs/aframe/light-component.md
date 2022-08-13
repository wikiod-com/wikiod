---
title: "light (component)"
slug: "light-component"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The light component defines the entity as a source of light. Light affects all materials that have not specified a flat shading model with shader: flat. Note that lights are computationally expensive we should limit number of lights in a scene.

## Syntax
 - &lt;a-entity light="color: #AFA; intensity: 1.5" position="-1 1 0">&lt;/a-entity&gt;
 - &lt;a-light type="point" color="blue" position="0 5 0">&lt;/a-light>

## Parameters
| Parameters | Details                                               |
| ---------- | ----------------------------------------------------- |
| type       | One of ambient, directional, hemisphere, point, spot. |
| color      | Light color.                                          |
| intensity  | Light strength.                                       |

## Ambient
Ambient lights globally affect all entities in the scene. The color and intensity properties define ambient lights. Additionally, position, rotation, and scale have no effect on ambient lights.

We recommend to have some form of ambient light such that shadowed areas are not fully black and to mimic indirect lighting.

    <a-entity light="type: ambient; color: #CCC"></a-entity>

## Directional
Directional lights are like a light source that is infinitely far away, but shining from a specific direction, like the sun. Thus, absolute position do not have an effect on the intensity of the light on an entity. We can specify the direction using the position component.

The example below creates a light source shining from the upper-left at a 45-degree angle. Note that because only the vector matters, position="-100 100 0" and position="-1 1 0" are the same.

    <a-entity light="type: directional; color: #EEE; intensity: 0.5" position="-1 1 0"></a-entity>

We can specify the direction of the directional light with its orientation by creating a child entity it targets. For example, pointing down its -Z axis:

    <a-light type="directional" position="0 0 0" rotation="-90 0 0" target="#directionaltarget">
        <a-entity id="directionaltarget" position="0 0 -1"></a-entity>
    </a-light>

## Hemisphere
Hemisphere lights are like an ambient light, but with two different colors, one from above (color) and one from below (groundColor). This can be useful for scenes with two distinct lighting colors (e.g., a grassy field under a gray sky).

    <a-entity light="type: hemisphere; color: #33C; groundColor: #3C3; intensity: 2"></a-entity>

| Property    | Description             | Default Value
| ----------- | ----------------------- | -------------
| groundColor | Light color from below. | #fff

## Point
Point lights, unlike directional lights, are omni-directional and affect materials depending on their position and distance. Point likes are like light bulb. The closer the light bulb gets to an object, the greater the object is lit.

    <a-entity light="type: point; intensity: 0.75; distance: 50; decay: 2"
          position="0 10 10"></a-entity>

| Property | Description | Default Value |
| -------- | ----------- | ------------- |
| decay    | Amount the light dims along the distance of the light.   | 1.0
| distance | Distance where intensity becomes 0. If distance is 0, then the point light does not decay with distance.  | 0.0

## Spot
Spot lights are like point lights in the sense that they affect materials depending on its position and distance, but spot lights are not omni-directional. They mainly cast light in one direction, like the Bat-Signal.

    <a-entity light="type: spot; angle: 45"></a-entity>

Property  |  Description |   Default Value
--- | --- | ---
angle  |  Maximum extent of spot light from its direction (in degrees). |   60
decay   | Amount the light dims along the distance of the light.  |  1.0
distance  |  Distance where intensity becomes 0. If distance is 0, then the point light does not decay with distance.   | 0.0
penumbra  |  Percent of the spotlight cone that is attenuated due to penumbra. |   0.0
target  |  element the spot should point to. set to null to transform spotlight by orientation, pointing to it’s -Z axis.   | null

## Default lighting
By default, A-Frame scenes inject default lighting, an ambient light and a directional light. These default lights are visible in the DOM with the data-aframe-default-light attribute. Whenever we add any lights, A-Frame removes the default lights from the scene.

    <!-- Default lighting injected by A-Frame. -->
    <a-entity light="type: ambient; color: #BBB"></a-entity>
    <a-entity light="type: directional; color: #FFF; intensity: 0.6" position="-0.5 1 1"></a-entity>

