---
title: "Transforms"
slug: "transforms"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Syntax
- void Transform.Translate(Vector3 translation, Space relativeTo = Space.Self)
- void Transform.Translate(float x, float y, float z, Space relativeTo = Space.Self)
- void Transform.Rotate(Vector3 eulerAngles, Space relativeTo = Space.Self)
- void Transform.Rotate(float xAngle, float yAngle, float zAngle, Space relativeTo = Space.Self)
- void Transform.Rotate(Vector3 axis, float angle, Space relativeTo = Space.Self)
- void Transform.RotateAround(Vector3 point, Vector3 axis, float angle)
- void Transform.LookAt(Transform target, Vector3 worldUp = Vector3.up)
- void Transform.LookAt(Vector3 worldPosition, Vector3 worldUp = Vector3.up)

## Overview
Transforms hold the majority of data about an object in unity, including it's parent(s), child(s), position, rotation, and scale. It also has functions to modify each of these properties. Every GameObject has a Transform.

**Translating (moving) an object**
<!-- language-all: c# -->
    
    // Move an object 10 units in the positive x direction
    transform.Translate(10, 0, 0);
    
    // translating with a vector3
    vector3 distanceToMove = new Vector3(5, 2, 0);
    transform.Translate(distanceToMove);

**Rotating an object**

    // Rotate an object 45 degrees about the Y axis
    transform.Rotate(0, 45, 0);

    // Rotates an object about the axis passing through point (in world coordinates) by angle in degrees
    transform.RotateAround(point, axis, angle);
    // Rotates on it's place, on the Y axis, with 90 degrees per second
    transform.RotateAround(Vector3.zero, Vector3.up, 90 * Time.deltaTime);

    // Rotates an object to make it's forward vector point towards the other object
    transform.LookAt(otherTransform);
    // Rotates an object to make it's forward vector point towards the given position (in world coordinates)
    transform.LookAt(new Vector3(10, 5, 0));

More information and examples can be seen at [Unity documentation][1]. 

Also note that if the game is using rigid bodies, then the transform should not be interacted with directly (unless the rigid body has `isKinematic == true`). In those case  use [AddForce][2] or other similar methods to act on the rigid body directly.


  [1]: https://docs.unity3d.com/ScriptReference/Transform.html
  [2]: https://docs.unity3d.com/ScriptReference/Rigidbody.AddForce.html

## Parenting and Children
Unity works with hierarchies in order to keep your project organized. You can assign objects a place in the hierarchy using the editor but you can also do this through code.

**Parenting**

You can set an object's parent with the following methods
<!-- language-all: c# -->

    var other = GetOtherGameObject();
    other.transform.SetParent( transform );
    other.transform.SetParent( transform, worldPositionStays );

Whenever you set a transforms parent, it will keep the objects position as a world position. You can choose to make this position relative by passing *false* for the *worldPositionStays* parameter.

You can also check if the object is a child of another transform with the following method

    other.transform.IsChildOf( transform );

&nbsp;

**Getting a Child**

Since objects can be parented to one another, you can also find children in the hierarchy. The simplest way of doing this is by using the following method

    transform.Find( "other" );
    transform.FindChild( "other" );

*Note: FindChild calls Find under the hood*

You can also search for children further down the hierarchy. You do this by adding in a "/" to specify going a level deeper.

    transform.Find( "other/another" );
    transform.FindChild( "other/another" );

Another way of fetching a child is using the GetChild

    transform.GetChild( index );

> GetChild requires an integer as index which must be smaller than the total child count

    int count = transform.childCount;

&nbsp;

**Changing Sibling Index**

You can change the order of the children of a GameObject. You can do this to define the draw order of the children (assuming that they are on the same Z level and the same sorting order).

    other.transform.SetSiblingIndex( index );

You can also quickly set the sibling index to either first or last using the following methods

    other.transform.SetAsFirstSibling();
    other.transform.SetAsLastSibling();

&nbsp;

**Detaching all Children**

If you want to release all children of a transform, you can do this:

    foreach(Transform child in transform)
    {
        child.parent = null;
    }

Also, Unity provides a method for this purpose:

    transform.DetachChildren();

Basically, both looping and `DetachChildren()` set the parents of first-depth children to null - which means they will have no parents.

*(first-depth children: the transforms that are directly child of transform)*

