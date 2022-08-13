---
title: "Physics"
slug: "physics"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Rigidbodies
Overview
========
The Rigidbody component gives a GameObject a *physical presence* in the scene in that it is able to respond to forces. You could apply forces directly to the GameObject or allow it to react to external forces such as gravity or another Rigidbody hitting it.
<hr>

Adding a Rigidbody component 
============================
You can add a Rigidbody by clicking **Component > Physics > Rigidbody**
<hr>

Moving a Rigidbody object
=========================
It is recommended that if you apply a Rigidbody to a GameObject that you use forces or torque to move it rather than manipulating it's Transform. Use `AddForce()` or `AddTorque()` methods for this:
   
    // Add a force to the order of myForce in the forward direction of the Transform.
    GetComponent<Rigidbody>().AddForce(transform.forward * myForce);

    // Add torque about the Y axis to the order of myTurn.
    GetComponent<Rigidbody>().AddTorque(transform.up * torque * myTurn);
<hr>

Mass
====
You can alter the mass of a Rigidbody GameObject to affect how it reacts with other Rigidbodies and forces. A higher mass means the GameObject will have more of an influence on other physics-based GameObjects, and will require a greater force to move itself. Objects of differing mass will fall at the same rate if they have the same drag values. To alter mass in code:

    GetComponent<Rigidbody>().mass = 1000;
<hr>

Drag
====
The higher the drag value, the more an object will slow down while moving. Think of it like an opposing force. To alter drag in code:

    GetComponent<Rigidbody>().drag = 10;
<hr>

isKinematic
===========
If you mark a Rigidbody as **Kinematic** then it cannot be affected by other forces but can still affect other GameObjects. To alter in code:

    GetComponent<Rigidbody>().isKinematic = true;
<hr>

Constraints
===========
It is also possible to add constraints to each axis to freeze the Rigidbody's position or rotation in local space. The default is `RigidbodyConstraints.None` as shown here:

[![enter image description here][1]][1]

An example of constraints in code:

    // Freeze rotation on all axes.
    GetComponent<Rigidbody>().constraints = RigidbodyConstraints.FreezeRotation 

    // Freeze position on all axes.
    GetComponent<Rigidbody>().constraints = RigidbodyConstraints.FreezePosition 

    // Freeze rotation and motion an all axes.
    GetComponent<Rigidbody>().constraints = RigidbodyConstraints.FreezeAll 

You can use the bitwise OR operator `|` to combine multiple constraints like so:

    // Allow rotation on X and Y axes and motion on Y and Z axes.
    GetComponent<Rigidbody>().constraints = RigidbodyConstraints.FreezePositionZ | 
        RigidbodyConstraints.FreezeRotationX;
<hr>

Collisions
==========
If you want a GameObject with a Rigidbody on it to respond to collisions you will also need to add a collider to it. Types of collider are:

 - Box collider        
 - Sphere collider
 - Capsule collider
 - Wheel collider
 - Mesh collider

If you apply more than one collider to a GameObject, we call it a Compound collider.

You can make a collider into a **Trigger** in order to use the `OnTriggerEnter()`, `OnTriggerStay()` and `OnTriggerExit()` methods. A trigger collider will not physically react to collisions, other GameObjects simply pass through it. They are useful for detecting when another GameObject is in a certain area or not, for example, when collecting an item, we may want to be able to just run through it but detect when this happens.

  [1]: http://i.stack.imgur.com/e6PwA.png


## Gravity in Rigid Body
The `useGravity` property of a `RigidBody` controls whether gravity affects it or not. If set to `false` the `RigidBody` will behave as if in outer space (without a constant force being applied to it in some direction).

    GetComponent<RigidBody>().useGravity = false;

It is very useful in the situations where you need all other properties of `RigidBody` except the motion controlled by gravity.

When enabled, the `RigidBody` will be affected by a gravitational force, set up under `Physics Settings`:

[![enter image description here][1]][1]

Gravity is defined in world units per second squared, and is entered here as a three-dimensional vector: meaning that with the settings in the example image, all `RigidBodies` with the `useGravity` property set to `True` will experience a force of 9.81 world units per second _per second_ in the downwards direction (as negative Y-values in Unity's coordinate system point downwards).


  [1]: https://i.stack.imgur.com/nozwu.png

