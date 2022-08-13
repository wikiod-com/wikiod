---
title: "Design Patterns"
slug: "design-patterns"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Model View Controller (MVC) Design Pattern
The model view controller is a very common design pattern that has been around for quite some time. This pattern focuses on reducing *spaghetti* code by separating classes into functional parts. Recently I have been experimenting with this design pattern in Unity and would like to lay out a basic example.

A MVC design consists of three core parts: Model, View and Controller.


----------

**Model:** The model is a class representing the data portion of your object. This could be a player, inventory or an entire level. If programmed correctly, you should be able to take this script and use it outside of Unity.

Note a few things about the Model:

 - It should not inherit from Monobehaviour
 - It should not contain Unity specific code for portability
 - Since we are avoiding Unity API calls, this can hinder things like implicit converters in the Model class (workarounds are required)

**Player.cs**
    
    using System;

    public class Player
    {
        public delegate void PositionEvent(Vector3 position);
        public event PositionEvent OnPositionChanged;

        public Vector3 position 
        {
            get 
            {
                return _position;
            }
            set 
            {
                if (_position != value) {
                    _position = value;
                    if (OnPositionChanged != null) {
                        OnPositionChanged(value);
                    }
                }
            }
        }
        private Vector3 _position;
    }

**Vector3.cs**

A custom Vector3 class to use with our data model.

    using System;
    
    public class Vector3
    {
        public float x;
        public float y;
        public float z;

        public Vector3(float x, float y, float z)
        {
            this.x = x;
            this.y = y;
            this.z = z;
        }
    }

----------


**View:** The view is a class representing the viewing portion tied to the model. This is an appropriate class to derive from Monobehaviour. This should contain code that interacts directly with Unity specific APIs including `OnCollisinEnter`, `Start`, `Update`, etc...
 - Typically inherits from Monobehaviour
 - Contains Unity specific code

**PlayerView.cs**

    using UnityEngine;

    public class PlayerView : Monobehaviour
    {
        public void SetPosition(Vector3 position)
        {
            transform.position = position;
        }
    }

**Controller:** The controller is a class that binds together both the Model and View. Controllers keep both Model and View in sync as well as drive interaction. The controller can listen for events from either partner and update accordingly.
 - Binds both the Model and View by syncing state
 - Can drive interaction between partners
 - Controllers may or may not be portable (You might have to use Unity code here)
 - If you decide to not make your controller portable, consider making it a Monobehaviour to help with editor inspecting

**PlayerController.cs**

    using System;

    public class PlayerController
    {
        public Player model { get; private set; }
        public PlayerView view { get; private set; }

        public PlayerController(Player model, PlayerView view)
        {
            this.model = model;
            this.view = view;

            this.model.OnPositionChanged += OnPositionChanged;
        }

        private void OnPositionChanged(Vector3 position)
        {
            // Sync
            Vector3 pos = this.model.position;

            // Unity call required here! (we lost portability)
            this.view.SetPosition(new UnityEngine.Vector3(pos.x, pos.y, pos.z));
        }
        
        // Calling this will fire the OnPositionChanged event 
        private void SetPosition(Vector3 position)
        {
            this.model.position = position;
        }
    }


----------


**Final Usage**

Now that we have all of the main pieces, we can create a factory that will generate all three parts.

**PlayerFactory.cs**

    using System;

    public class PlayerFactory
    {
        public PlayerController controller { get; private set; }
        public Player model { get; private set; }
        public PlayerView view { get; private set; }

        public void Load()
        {
            // Put the Player prefab inside the 'Resources' folder
            // Make sure it has the 'PlayerView' Component attached
            GameObject prefab = Resources.Load<GameObject>("Player");
            GameObject instance = GameObject.Instantiate<GameObject>(prefab);
            this.model = new Player();
            this.view = instance.GetComponent<PlayerView>();
            this.controller = new PlayerController(model, view);
        }
    }

And finally we can call the factory from a manager...

**Manager.cs**

    using UnityEngine;

    public class Manager : Monobehaviour
    {
        [ContextMenu("Load Player")]
        private void LoadPlayer()
        {
            new PlayerFactory().Load();
        }
    }

Attach the Manager script to an empty GameObject in the scene, right click the component and select "Load Player".

For more complex logic you can introduce inheritance with abstract base classes and interfaces for an improved architecture.

