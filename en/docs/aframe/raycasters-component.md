---
title: "Raycasters (component)"
slug: "raycasters-component"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

The raycaster component does general intersection testing with a raycaster. Raycasting is the method of extending a line from an origin towards a direction, and checking whether that line intersects with other entites. The raycaster component is a wrapper on top of the three.js raycaster. It checks for intersections at a certain interval against a list of objects, and will emit events on the entity when it detects intersections or clearing of intersections (i.e., when the raycaster is no longer 

## Parameters
| Parameter | Details | 
| --------- | ------- |
| far     | Maximum distance under which resulting entities are returned. Cannot be lower then near. |
| interval | Number of milliseconds to wait in between each intersection test. Lower number is better for faster updates. Higher number is better for performance.   | 
| near  |  Minimum distance over which resuilting entities are returned. Cannot be lower than 0.  |  
| objects |   Query selector to pick which objects to test for intersection. If not specified, all entities will be tested.   |
| recursive |   Checks all children of objects if set. Else only checks intersections with root objects.  |  



# Events

Name   | Details
------ | --------
`raycaster-intersected`   | Emitted on the intersected entity. Entity is intersecting with a raycaster. Event detail will contain el, the raycasting entity, and intersection, an object containing detailed data about the intersection.
`raycaster-intersected-cleared`  |  Emitted on the intersected entity. Entity is no longer intersecting with a raycaster. Event detail will contain el, the raycasting entity.
`raycaster-intersection`  |  Emitted on the raycasting entity. Raycaster is intersecting with one or more entities. Event detail will contain els, an array with the intersected entities, and intersections, an array of objects containing detailed data about the intersections.
`raycaster-intersection-cleared`   | Emitted on the raycasting entity. Raycaster is no longer intersecting with an entity. Event detail will contain el, the formerly intersected entity.

# Member

Member |   Description
--- | ---
`intersectedEls` |   Entities currently intersecting the raycaster.
`objects` |    three.js objects to test for intersections. Will be scene.children if not objects property is not specified.
`raycaster` |   three.js raycaster object.

# Methode

Method  |  Description
--- | ---
`refreshObjects` |    Refreshes the list of objects based off of the objects property to test for intersection.

## Setting the Origin and Direction of the Raycaster
The raycaster has an origin, where its ray starts, and a direction, where the ray goes.

The origin of the raycaster is at the raycaster entity’s position. We can change the origin of the raycaster by setting the position component of the raycaster entity (or parent entities of the raycaster entity).

The direction of the raycaster is in “front” of the raycaster entity (i.e., 0 0 -1, on the negative Z-axis). We can change the direction of the raycaster by setting the rotation component of the raycaster entity (or parent entities of the raycaster entity).

For example, here is applying a raycaster along the length of a rotated bullet:

<!-- language: lang-html -->

    <!-- Bullet, rotated to be parallel with the ground. -->
    <a-entity id="bullet" geometry="primitive: cylinder; height: 0.1" rotation="-90 0 0">
      <!-- Raycaster, targets enemies, made to be as long as the bullet, positioned to the start of the bullet, rotated to align with the bullet. -->
      <a-entity raycaster="objects: .enemies; far: 0.1" position="0 -0.5 0" rotation="90 0 0"></a-entity>
    </a-entity>

## Whitelisting Entities to Test for Intersection
We usually don’t want to test everything in the scene for intersections (e.g., for collisions or for clicks). Selective intersections are good for performance to limit the number of entities to test for intersection since intersection testing is an operation that will run over 60 times per second.

To select or pick the entities we want to test for intersection, we can use the objects property. If this property is not defined, then the raycaster will test every object in the scene for intersection. objects takes a query selector value:

<!-- language: lang-html -->

    <a-entity raycaster="objects: .clickable" cursor></a-entity>
    <a-entity class="clickable" geometry="primitive: box" position="1 0 0"></a-entity>
    <a-entity class="not-clickable" geometry="primitive: sphere" position="-1 0 0"></a-entity>



