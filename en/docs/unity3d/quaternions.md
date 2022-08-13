---
title: "Quaternions"
slug: "quaternions"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## Syntax
 - Quaternion.LookRotation(Vector3 forward [, Vector3 up]);
 - Quaternion.AngleAxis(float angles, Vector3 axisOfRotation);
 - float angleBetween = Quaternion.Angle(Quaternion rotation1, Quaternion rotation2);

## Intro to Quaternion vs Euler
Euler angles are "degree angles" like 90, 180, 45, 30 degrees. Quaternions differ from Euler angles in that they represent a point on a Unit Sphere (the radius is 1 unit). You can think of this sphere as a 3D version of the Unit circle you learn in trigonometry. 
Quaternions differ from Euler angles in that they use imaginary numbers to define a 3D rotation. 

While this may sound complicated (and arguably it is), Unity has great builtin functions that allow you to switch between Euler angles and quaterions, as well as functions to modify quaternions, without knowing a single thing about the math behind them.

**Converting Between Euler and Quaternion**
<!-- language-all: c# -->
    
    // Create a quaternion that represents 30 degrees about X, 10 degrees about Y
    Quaternion rotation = Quaternion.Euler(30, 10, 0);

    // Using a Vector
    Vector3 EulerRotation = new Vector3(30, 10, 0);
    Quaternion rotation = Quaternion.Euler(EulerRotation);

    // Convert a transfroms Quaternion angles to Euler angles
    Quaternion quaternionAngles = transform.rotation;
    Vector3 eulerAngles = quaternionAngles.eulerAngles;

**Why Use a Quaternion?**

Quaternions solve a problem known as gimbal locking. This occurs when the primary axis of rotation becomes collinear with the tertiary axis of rotation. Here's a [visual example] [1] @ 2:09


  [1]: https://youtu.be/zc8b2Jo7mno?t=2m9s

## Quaternion Look Rotation
`Quaternion.LookRotation(Vector3 forward [, Vector3 up])` will create a Quaternion rotation that looks forward 'down' the forward vector and has the Y axis aligned with the 'up' vector. If the up vector is not specified, Vector3.up will be used.

**Rotate this Game Object to look at a target Game Object**
<!-- language-all: c# -->

    // Find a game object in the scene named Target
    public Transform target = GameObject.Find("Target").GetComponent<Transform>();
    
    // We subtract our position from the target position to create a
    // Vector that points from our position to the target position
    // If we reverse the order, our rotation would be 180 degrees off.
    Vector3 lookVector = target.position - transform.position;
    Quaternion rotation = Quaternion.LookRotation(lookVector);
    transform.rotation = rotation;

