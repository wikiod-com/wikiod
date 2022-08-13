---
title: "Mobile platforms"
slug: "mobile-platforms"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
- public static int Input.touchCount
- public static Touch Input.GetTouch(int index)

## Detecting Touch
To detect a touch in Unity it's quite simple we just have to use `Input.GetTouch()` and pass it an index.

<!-- language: c# -->

    using UnityEngine;
    using System.Collections;

    public class TouchExample : MonoBehaviour { 
        void Update() {
            if (Input.touchCount > 0 && Input.GetTouch(0).phase == TouchPhase.Began)
            {
                //Do Stuff
            }
        }
    }
or

<!-- language: c# -->

    using UnityEngine;
    using System.Collections;

    public class TouchExample : MonoBehaviour { 
        void Update() {
            for(int i = 0; i < Input.touchCount; i++)
            {
                if (Input.GetTouch(i).phase == TouchPhase.Began)
                {
                    //Do Stuff
                }
            }    
        }
    }

These examples gets the touch of the last game frame.

TouchPhase
==========
----------
Inside of the TouchPhase enum there are 5 different kind of TouchPhase's

 - Began - a finger touched the screen
 - Moved - a finger moved on the screen
 - Stationary - a finger is on the screen but is not moving
 - Ended - a finger was lifted from the screen
 - Canceled - the system cancelled tracking for the touch

For example to move the object this script is attached to across the screen based on touch.

<!-- language: c# -->

    public class TouchMoveExample : MonoBehaviour 
    {
        public float speed = 0.1f;

        void Update () {
            if(Input.touchCount > 0 && Input.GetTouch(0).phase == TouchPhase.Moved)
            {
                Vector2 touchDeltaPosition = Input.GetTouch(0).deltaPosition;
                transform.Translate(-touchDeltaPosition.x * speed, -touchDeltaPosition.y * speed, 0);
            }
        }
    }

