---
title: "Vector3"
slug: "vector3"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

The `Vector3` structure represents a 3D coordinate, and is one of the backbone structures of the `UnityEngine` library. The `Vector3` structure is most commonly found in the `Transform` component of most game objects, where it is used to hold *position* and *scale*. `Vector3` provides good functionality for performing common vector operations.

[You can read more on the `Vector3` structure in the Unity API.](https://docs.unity3d.com/ScriptReference/Vector3.html)

## Syntax
- public Vector3();
- public Vector3(float x, float y);
- public Vector3(float x, float y, float z);
- Vector3.Lerp(Vector3 startPosition, Vector3 targetPosition, float movementFraction);
- Vector3.LerpUnclamped(Vector3 startPosition, Vector3 targetPosition, float movementFraction);
- Vector3.MoveTowards(Vector3 startPosition, Vector3 targetPosition, float distance);

## Static Values
The `Vector3` structure contains some static variables that provide commonly used `Vector3` values. Most represent a *direction*, but they can still be used creatively to provide additional functionality.

---
---

# `Vector3.zero` and `Vector3.one`

`Vector3.zero` and `Vector3.one` are typically used in connection to a *normalised* `Vector3`; that is, a `Vector3` where the `x`, `y` and `z` values have a magnitude of 1. As such, `Vector3.zero` represents the lowest value, whilst `Vector3.one` represents the largest value.

`Vector3.zero` is also commonly used to set the default position on object transforms.

---

The following class uses `Vector3.zero` and `Vector3.one` to inflate and deflate a sphere.

    using UnityEngine;

    public class Inflater : MonoBehaviour 
    {
        <summary>A sphere set up to inflate and deflate between two values.</summary>
        public ScaleBetween sphere;

        ///<summary>On start, set the sphere GameObject up to inflate
        /// and deflate to the corresponding values.</summary>
        void Start()
        {
            // Vector3.zero = Vector3(0, 0, 0); Vector3.one = Vector3(1, 1, 1);
            sphere.SetScale(Vector3.zero, Vector3.one);
        }
    }

---

[![A sphere inflated and deflated between Vector3.zero and Vector3.one][3]][3]

---
---

# Static Directions

The static directions can be useful in a number of applications, with direction along the positive and negative of all three axis. It is important to note that Unity employs a left-handed coordinate system, which has an affect on direction.

[![In the left-handed coordinate system, the X axis moves to the right, the Y axis moves upwards and the Z axis moves further inwards.][1]][1]

--------------------

The following class uses the static `Vector3` directions to move objects along the three axis.

    using UnityEngine;

    public class StaticMover : MonoBehaviour 
    {
        <summary>GameObjects set up to move back and forth between two directions.</summary>
        public MoveBetween xMovement, yMovement, zMovement;

        ///<summary>On start, set each MoveBetween GameObject up to move
        /// in the corresponding direction(s).</summary>
        void Start()
        {
            // Vector3.left = Vector3(-1, 0, 0); Vector3.right = Vector3(1, 0, 0);
            xMovement.SetDirections(Vector3.left, Vector3.right);

            // Vector3.down = Vector3(0, -1, 0); Vector3.up = Vector3(0, 0, 1);
            yMovement.SetDirections(Vector3.down, Vector3.up);

            // Vector3.back = Vector3(0, 0, -1); Vector3.forward = Vector3(0, 0, 1);
            zMovement.SetDirections(Vector3.back, Vector3.forward);
        }
    }

----

[![Animated cubes moving in the static directions.][2]][2]

---
---

# Index



| Value            |   x   |   y   |   z   | Equivalent `new Vector3()` method
| ---------------  | :---: | :---: | :---: | :-------------------------------:
| `Vector3.zero`   |   0   |   0   |   0   | `new Vector3(0, 0, 0)`
| `Vector3.one`    |   1   |   1   |   1   | `new Vector3(1, 1, 1)`
| `Vector3.left`   |  -1   |   0   |   0   | `new Vector3(-1, 0, 0)`
| `Vector3.right`  |   1   |   0   |   0   | `new Vector3(1, 0, 0)`
| `Vector3.down`   |   0   |  -1   |   0   | `new Vector3(0, -1, 0)`
| `Vector3.up`     |   0   |   1   |   0   | `new Vector3(0, 1, 0)`
| `Vector3.back`   |   0   |   0   |  -1   | `new Vector3(0, 0, -1)`
| `Vector3.forward`|   0   |   0   |   1   | `new Vector3(0, 0, 1)`

  [1]: https://i.stack.imgur.com/Ta1hi.png "The right-handed coordinate system versus the left-handed coordinate system"
  [2]: https://i.stack.imgur.com/KFlAD.gif "Moving cubes along the X, Y and Z axis using static Vector3 direction values."
  [3]: https://i.stack.imgur.com/B42Db.gif "Inflating and deflating a sphere using Vector3.zero and Vector3.one"

## Creating a Vector3
A `Vector3` structure can be created in several ways. `Vector3` is a struct, and as such, will typically need to be instantiated before use. 

---

# Constructors

There are three built in constructors for instantiating a `Vector3`.


| Constructor| Result|
| ------ | ------ |
| `new Vector3()`| Creates a `Vector3` structure with co-ordinates of (0, 0, 0). |
| `new Vector3(float x, float y)` | Creates a `Vector3` structure with the given `x` and `y` co-ordinates. `z` will be set to 0. |
| `new Vector3(float x, float y, float z)` | Creates a `Vector3` structure with the given `x`, `y` and `z` co-ordinates. |

---
---

# Converting from a `Vector2` or `Vector4`

While rare, you may run into situations where you would need to treat the co-ordinates of a `Vector2` or `Vector4` structure as a `Vector3`. In such cases, you can simply pass the `Vector2` or `Vector4` directly into the `Vector3`, without previously instantiating it. As should be assumed, a `Vector2` struct will only pass `x` and `y` values, while a `Vector4` class will omit its `w`.

---

We can see direct conversion in the below script.

    void VectorConversionTest()
    {
        Vector2 vector2 = new Vector2(50, 100);
        Vector4 vector4 = new Vector4(50, 100, 200, 400);

        Vector3 fromVector2 = vector2;
        Vector3 fromVector4 = vector4;

        Debug.Log("Vector2 conversion: " + fromVector2);
        Debug.Log("Vector4 conversion: " + fromVector4);
    }

---

[![Debug output confirms that both Vector2 and Vector4 have been successfully converted to Vector3.][1]][1]


  [1]: https://i.stack.imgur.com/4efdP.png "As you can see, the debug log confirms the conversion, and has no problem implicitly outputting the values we intend."

## Applying Movement
The `Vector3` structure contains some static functions that can provide utility when we wish to apply movement to the `Vector3`.

## [`Lerp`][1] and [`LerpUnclamped`][2]

The lerp functions provide movement between two co-ordinates based off a provided fraction. Where `Lerp` will only permit movement between the two co-ordinates, `LerpUnclamped` allows for fractions that move outside of the boundaries between the two co-ordinates.

We provide the fraction of movement as a `float`. With a value of `0.5`, we find the midpoint between the two `Vector3` co-ordinates. A value of `0` or `1` will return the first or second `Vector3`, respectivley, as these values either correlate to no movement (thus returning the first `Vector3`), or completed movement (this returning the second `Vector3`). It is important to note that neither function will accommodate for change in the movement fraction. This is something we need to manually account for.

With `Lerp`, all values are clamped between `0` and `1`. This is useful when we want to provide movement towards a direction, and do not want to overshoot the destination. `LerpUnclamped` can take any value, and can be used to provide movement *away* from the destination, or *past* the destination.

---

The following script uses `Lerp` and `LerpUnclamped` to move an object at a consistent pace.

    using UnityEngine;
    
    public class Lerping : MonoBehaviour
    {
        /// <summary>The red box will use Lerp to move. We will link
        /// this object in via the inspector.</summary>
        public GameObject lerpObject;
        /// <summary>The starting position for our red box.</summary>
        public Vector3 lerpStart = new Vector3(0, 0, 0);
        /// <summary>The end position for our red box.</summary>
        public Vector3 lerpTarget = new Vector3(5, 0, 0);
    
        /// <summary>The blue box will use LerpUnclamped to move. We will 
        /// link this object in via the inspector.</summary>
        public GameObject lerpUnclampedObject;
        /// <summary>The starting position for our blue box.</summary>
        public Vector3 lerpUnclampedStart = new Vector3(0, 3, 0);
        /// <summary>The end position for our blue box.</summary>
        public Vector3 lerpUnclampedTarget = new Vector3(5, 3, 0);
    
        /// <summary>The current fraction to increment our lerp functions by.</summary>
        public float lerpFraction = 0;
    
        private void Update()
        {
            // First, I increment the lerp fraction. 
            // delaTime * 0.25 should give me a value of +1 every second.
            lerpFraction += (Time.deltaTime * 0.25f);
    
            // Next, we apply the new lerp values to the target transform position.
            lerpObject.transform.position 
                = Vector3.Lerp(lerpStart, lerpTarget, lerpFraction);
            lerpUnclampedObject.transform.position 
                = Vector3.LerpUnclamped(lerpUnclampedStart, lerpUnclampedTarget, lerpFraction);
        }
    }

---

[![The red box moves to the target position, and stops. The blue box continues moving, indefinitely.][3]][3]

---
---

## [`MoveTowards`][4]

`MoveTowards` behaves *very similar* to `Lerp`; the core difference is that we provide an actual *distance* to move, instead of a *fraction* between two points. It is important to note that `MoveTowards` will not extend past the target `Vector3`. 

Much like with `LerpUnclamped`, we can provide a *negative* distance value to move *away* from the target `Vector3`. In such cases, we never move past the target `Vector3`, and thus movement is indefinite. In these cases, we can treat the target `Vector3` as an "opposite direction"; as long as the `Vector3` points in the same direction, relative to the start `Vector3`, negative movement should behave as normal.

---

The following script uses `MoveTowards` to move a group of objects towards a set of positions using a smoothed distance. 

    using UnityEngine;
        
    public class MoveTowardsExample : MonoBehaviour
    {
        /// <summary>The red cube will move up, the blue cube will move down, 
        /// the green cube will move left and the yellow cube will move right.
        /// These objects will be linked via the inspector.</summary>
        public GameObject upCube, downCube, leftCube, rightCube;
        /// <summary>The cubes should move at 1 unit per second.</summary>
        float speed = 1f;
    
        void Update()
        {
            // We determine our distance by applying a deltaTime scale to our speed.
            float distance = speed * Time.deltaTime;
    
            // The up cube will move upwards, until it reaches the 
            //position of (Vector3.up * 2), or (0, 2, 0).
            upCube.transform.position 
                = Vector3.MoveTowards(upCube.transform.position, (Vector3.up * 2f), distance);
    
            // The down cube will move downwards, as it enforces a negative distance..
            downCube.transform.position
                = Vector3.MoveTowards(downCube.transform.position, Vector3.up * 2f, -distance);
    
            // The right cube will move to the right, indefinetly, as it is constantly updating
            // its target position with a direction based off the current position.
            rightCube.transform.position = Vector3.MoveTowards(rightCube.transform.position, 
                rightCube.transform.position + Vector3.right, distance);
    
            // The left cube does not need to account for updating its target position, 
            // as it is moving away from the target position, and will never reach it.
            leftCube.transform.position
                = Vector3.MoveTowards(leftCube.transform.position, Vector3.right, -distance);
        }
    }

---

[![All cubes move outwards from the center, with the red cube stopping at its target destination.][5]][5]

---
---

## [`SmoothDamp`][6]

Think of `SmoothDamp` as a variant of `MoveTowards` with built in smoothing. According to official documentation, this function is most commonly used to perform smooth camera following.

Along with the start and target `Vector3` coordinates, we must also provide a `Vector3` to represent the velocity, and a `float` representing the *approximate* time it should take to complete the movement. Unlike previous examples, we provide the velocity as a *reference*, to be incremented, internally. It is important to take note of this, as changing velocity outside of the function while we are still performing the function can have undesired results.

In addition to the *required* variables, we may also provide a `float` to represent the maximum speed of our object, and a `float` to represent the time gap since the previous `SmoothDamp` call to the object. We do not *need* to provide these values; by default, there will be no maximum speed, and the time gap will be interpretted as `Time.deltaTime`. More importantly, if you are calling the function one per object inside a `MonoBehaviour.Update()` function, you should *not* need to declare a time gap.

---

    using UnityEngine;
        
    public class SmoothDampMovement : MonoBehaviour
    {
        /// <summary>The red cube will imitate the default SmoothDamp function. 
        /// The blue cube will move faster by manipulating the "time gap", while 
        /// the green cube will have an enforced maximum speed. Note that these 
        /// objects have been linked via the inspector.</summary>
        public GameObject smoothObject, fastSmoothObject, cappedSmoothObject;
    
        /// <summary>We must instantiate the velocities, externally, so they may 
        /// be manipulated from within the function. Note that by making these 
        /// vectors public, they will be automatically instantiated as Vector3.Zero 
        /// through the inspector. This also allows us to view the velocities, 
        /// from the inspector, to observe how they change.</summary>
        public Vector3 regularVelocity, fastVelocity, cappedVelocity;
    
        /// <summary>Each object should move 10 units along the X-axis.</summary>
        Vector3 regularTarget = new Vector3(10f, 0f);
        Vector3 fastTarget = new Vector3(10f, 1.5f);
        Vector3 cappedTarget = new Vector3(10f, 3f);
    
        /// <summary>We will give a target time of 5 seconds.</summary>
        float targetTime = 5f;
    
        void Update()
        {
            // The default SmoothDamp function will give us a general smooth movement.
            smoothObject.transform.position = Vector3.SmoothDamp(smoothObject.transform.position,
                regularTarget, ref regularVelocity, targetTime);
    
            // Note that a "maxSpeed" outside of reasonable limitations should not have any 
            // effect, while providing a "deltaTime" of 0 tells the function that no time has 
            // passed since the last SmoothDamp call, resulting in no movement, the second time.
            smoothObject.transform.position = Vector3.SmoothDamp(smoothObject.transform.position,
                regularTarget, ref regularVelocity, targetTime, 10f, 0f);
    
            // Note that "deltaTime" defaults to Time.deltaTime due to an assumption that this 
            // function will be called once per update function. We can call the function 
            // multiple times during an update function, but the function will assume that enough
            // time has passed to continue the same approximate movement. As a result, 
            // this object should reach the target, quicker.
            fastSmoothObject.transform.position = Vector3.SmoothDamp(
                fastSmoothObject.transform.position, fastTarget, ref fastVelocity, targetTime);
            fastSmoothObject.transform.position = Vector3.SmoothDamp(
                fastSmoothObject.transform.position, fastTarget, ref fastVelocity, targetTime);
    
            // Lastly, note that a "maxSpeed" becomes irrelevant, if the object does not 
            // realistically reach such speeds. Linear speed can be determined as 
            // (Distance / Time), but given the simple fact that we start and end slow, we can 
            // infer that speed will actually be higher, during the middle. As such, we can
            // infer that a value of (Distance / Time) or (10/5) will affect the 
            // function. We will half the "maxSpeed", again, to make it more noticeable.
            cappedSmoothObject.transform.position = Vector3.SmoothDamp(
                cappedSmoothObject.transform.position, 
                cappedTarget, ref cappedVelocity, targetTime, 1f);
        }
    }

---

[![We can alter the speed at which the object reaches the target by adjusting the maxSpeed and deltaTime parameters.][7]][7]



  [1]: https://docs.unity3d.com/ScriptReference/Vector3.Lerp.html "Vector3.Lerp @ docs.unity3d.com"
  [2]: https://docs.unity3d.com/ScriptReference/Vector3.LerpUnclamped.html "Vector3.LerpUnclamped @ docs.unity3d.com"
  [3]: https://i.stack.imgur.com/BIntm.gif "Moving cubes using the Lerp functions."
  [4]: https://docs.unity3d.com/ScriptReference/Vector3.MoveTowards.html "Vector3.MoveTowards @ docs.unity3d.com"
  [5]: https://i.stack.imgur.com/m8EzX.gif "Moving cubes using an assortment of implementations of the MoveTowards function."
  [6]: https://docs.unity3d.com/ScriptReference/Vector3.SmoothDamp.html "Vector3.SmoothDamp @ docs.unity3d.com"
  [7]: https://i.stack.imgur.com/4cSuY.gif "Playing with the maxSpeed and deltaTime parameters has a clear affect on the speed of our objects."

