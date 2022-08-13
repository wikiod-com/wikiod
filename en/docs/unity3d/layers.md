---
title: "Layers"
slug: "layers"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Layer usage
Unity layers are similar to tags as in that they can be used to define objects that should be interacted with or should behave in a certain manner, however, layers are mainly used with functions in the `Physics` class: [Unity Documentation - Physics][1]

Layers are represented by an integer and can be passed to the functions in this manner:

<!-- language: c# -->

    using UnityEngine;
    class LayerExample {

        public int layer;

        void Start()
        {
            Collider[] colliders = Physics.OverlapSphere(transform.position, 5f, layer);
        }
    }
Using a layer in this manner will include only Colliders whose GameObjects have the layer specified in the calculations done. This makes further logic simpler as well as improving performance. 


  [1]: https://docs.unity3d.com/ScriptReference/Physics.html

## LayerMask Structure
The `LayerMask` structure is an interface that functions almost exactly like passing an integer to the function in question. However, its biggest benefit is allowing the user to select the layer in question from a drop-down menu in the inspector.

<!-- language: c# -->

    using UnityEngine;
    class LayerMaskExample{

        public LayerMask mask;
        public Vector3 direction;
    
        void Start()
        {
            if(Physics.Raycast(transform.position, direction, 35f, mask))
            {
                Debug.Log("Raycast hit");
            }
        {
    }

It also has multiple static functions that allow for converting layer names to indices or indices to layer names.

<!-- language: c# -->

    using UnityEngine;
    class NameToLayerExample{
    
        void Start()
        {
            int layerindex = LayerMask.NameToLayer("Obstacle");
        {
    }
In order to make Layer checking easy define the following extension method. 

<!-- language: c# -->

    public static bool IsInLayerMask(this GameObject @object, LayerMask layerMask)
    {
        bool result = (1 << @object.layer & layerMask) == 0;
    
        return result;
    }

This method will allow you to check whether a gameobject is in a layermask (selected in the editor) or not. 

