---
title: "Raycast"
slug: "raycast"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
|  origin  | The starting point of the ray in world coordinates   |
| direction | The direction of the ray|
| maxDistance | The max distance the ray should check for collisions|
| layerMask | A Layer mask that is used to selectively ignore Colliders when casting a ray. |
| queryTriggerInteraction| Specifies where this query should hit Triggers. |

## Encapsulating Raycast calls
Having your scripts call `Raycast` directly may lead to problems if you need to change the collision matrices in the future, as you'll have to track down every `LayerMask` field to accommodate the changes. Depending on the size of your project, this may become a huge undertaking.

Encapsulating `Raycast` calls may make your life easier down the line.

Looking at it from a [SoC][1] principle, a gameobject really shouldn't know or care about LayerMasks. It only needs a method to scan its surroundings. Whether the raycast result returns this or that shouldn't matter to the gameobject. It should only act upon the information it receives and not make any assumptions on the environment it exists in.

One way to approach this is to move the LayerMask value to [ScriptableObject][2] instances and use those as a form of raycast services that you inject into your scripts.

<!-- language: c# -->
    // RaycastService.cs
    using UnityEngine;
    
    [CreateAssetMenu(menuName = "StackOverflow")]
    public class RaycastService : ScriptableObject
    {
        [SerializeField]
        LayerMask layerMask;

        public RaycastHit2D Raycast2D(Vector2 origin, Vector2 direction, float distance)
        {
            return Physics2D.Raycast(origin, direction, distance, layerMask.value);
        }

        // Add more methods as needed

    }

<!-- language: c# -->
    // MyScript.cs
    using UnityEngine;
    
    public class MyScript : MonoBehaviour
    {
        [SerializeField]
        RaycastService raycastService;

        void FixedUpdate()
        {
            RaycastHit2D hit = raycastService.Raycast2D(Vector2.zero, Vector2.down, 1f);
        }
    }

This allows you to make a number of raycast services, all with different LayerMask combinations for different situations. You could have one that hits only ground colliders, and another that hits ground colliders and one way platforms.

If you ever need to make drastic changes to your LayerMask setups, you only need to update these RaycastService assets.

Further reading
---------------
 - [Inversion of control][3]
 - [Dependency injection][4]


  [1]: https://en.wikipedia.org/wiki/Separation_of_concerns
  [2]: https://www.wikiod.com/unity3d/scriptableobject
  [3]: https://en.wikipedia.org/wiki/Inversion_of_control
  [4]: https://en.wikipedia.org/wiki/Dependency_injection

## Physics Raycast
This function casts a ray from point `origin` in direction `direction` of length `maxDistance` against all colliders in the scene.

The function takes in the `origin` `direction` `maxDistance` and calculate if there is a collider in front of the GameObject.

    Physics.Raycast(origin, direction, maxDistance);

For example, this function will print `Hello World` to the console if there is something within 10 units of the `GameObject` attached to it:

    using UnityEngine;
    
    public class TestPhysicsRaycast: MonoBehaviour 
    {
        void FixedUpdate() 
        {
            Vector3 fwd = transform.TransformDirection(Vector3.forward);
            
            if (Physics.Raycast(transform.position, fwd, 10)) 
                print("Hello World");
        }
    }

## Physics2D Raycast2D
You can use raycasts to check if an ai can walk without falling off the edge of a level.

    using UnityEngine;
        
    public class Physics2dRaycast: MonoBehaviour 
        {
            public LayerMask LineOfSightMask;
            void FixedUpdate() 
            {
                RaycastHit2D hit = Physics2D.Raycast(raycastRightPart, Vector2.down, 0.6f * heightCharacter, LineOfSightMask);
                if(hit.collider != null)
                {
                    //code when the ai can walk
                }
                else
                {
                    //code when the ai cannot walk
                }
        }
    }

        
        
In this example the direction is right. The variable raycastRightPart is the right part of the character, so the raycast will happen at the right part of the character. The distance is 0.6f times the height of the character so the raycast won't give a hit when he hits the ground that is way lower than the ground he is standing on at the moment.
Make sure the Layermask is set to ground only, otherwise it will detect other kinds of objects as well.

> RaycastHit2D itself is a structure and not a class so hit can't be null; this means you have to check for the collider of a RaycastHit2D variable.

