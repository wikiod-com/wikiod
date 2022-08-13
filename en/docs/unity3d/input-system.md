---
title: "Input System"
slug: "input-system"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Reading Key Press and difference between GetKey, GetKeyDown and GetKeyUp
Input must must read from the Update function.

Reference for all the  available [Keycode][1] enum.

**1.** Reading key press with `Input.GetKey`:

`Input.GetKey` will **repeatedly** return `true` while the user holds down the specified key. This can be used to **repeatedly** fire a weapon while holding the specified key down. Below is an example of bullet auto-fire when the Space key is held down. The player doesn't have to press and release the key over and over again.

<!-- language: c# -->

    public GameObject bulletPrefab;
    public float shootForce = 50f;
    
    void Update()
    {
        if (Input.GetKey(KeyCode.Space))
        {
            Debug.Log("Shooting a bullet while SpaceBar is held down");
    
            //Instantiate bullet
            GameObject bullet = Instantiate(bulletPrefab, transform.position, transform.rotation) as GameObject;
    
            //Get the Rigidbody from the bullet then add a force to the bullet
            bullet.GetComponent<Rigidbody>().AddForce(bullet.transform.forward * shootForce);
        }
    }

**2**.Reading key press with `Input.GetKeyDown`:

`Input.GetKeyDown` will true only **once** when the specified key is pressed. This is the key difference between `Input.GetKey` and `Input.GetKeyDown`. One example use of its use is to toggle a UI or flashlight or an item on/off. 

<!-- language: c# -->

    public Light flashLight;
    bool enableFlashLight = false;
    
    void Update()
    {
        if (Input.GetKeyDown(KeyCode.Space))
        {
            //Toggle Light 
            enableFlashLight = !enableFlashLight;
            if (enableFlashLight)
            {
                flashLight.enabled = true;
                Debug.Log("Light Enabled!");
            }
            else
            {
                flashLight.enabled = false;
                Debug.Log("Light Disabled!");
            }
        }
    }

**3**.Reading key press with `Input.GetKeyUp`:

This is the exact opposite of `Input.GetKeyDown`. It is used to detect when key-press is released/lifted. Just like `Input.GetKeyDown`, it returns `true` only **once**. For example, you can `enable` light when key is held down with `Input.GetKeyDown` then disable the light when key is released with `Input.GetKeyUp`.

<!-- language: c# -->

    public Light flashLight;
    void Update()
    {
        //Disable Light when Space Key is pressed
        if (Input.GetKeyDown(KeyCode.Space))
        {
            flashLight.enabled = true;
            Debug.Log("Light Enabled!");
        }
    
        //Disable Light when Space Key is released
        if (Input.GetKeyUp(KeyCode.Space))
        {
            flashLight.enabled = false;
            Debug.Log("Light Disabled!");
        }
    }



  [1]: https://docs.unity3d.com/ScriptReference/KeyCode.html

## Read Mouse Button ( Left, Middle, Right) Clicks
These functions are used to check Mouse Button Clicks.

<!-- language-all: lang-cs -->

 - `Input.GetMouseButton(int button);`
 - `Input.GetMouseButtonDown(int button);`
 - `Input.GetMouseButtonUp(int button);`

They all take the-same parameter.

 - 0 = Left Mouse Click.
 - 1 = Right Mouse Click.
 - 2 = Middle Mouse Click.

`GetMouseButton` is used to detect when mouse button is *continuously held* down. It returns `true` while the specified mouse button is being held down.

    void Update()
    {
        if (Input.GetMouseButton(0))
        {
            Debug.Log("Left Mouse Button Down");
        }
    
        if (Input.GetMouseButton(1))
        {
            Debug.Log("Right Mouse Button Down");
        }
    
        if (Input.GetMouseButton(2))
        {
            Debug.Log("Middle Mouse Button Down");
        }
    }

`GetMouseButtonDown` is used to detect when there is mouse click. It returns `true` if it is pressed **once**. It won't return `true` again until the mouse button is released and pressed again.

    void Update()
    {
        if (Input.GetMouseButtonDown(0))
        {
            Debug.Log("Left Mouse Button Clicked");
        }
    
        if (Input.GetMouseButtonDown(1))
        {
            Debug.Log("Right Mouse Button Clicked");
        }
    
        if (Input.GetMouseButtonDown(2))
        {
            Debug.Log("Middle Mouse Button Clicked");
        }
    }

`GetMouseButtonUp` is used to detect when the specififed mouse button is released. This is will only return `true` once the specified mouse button is released. To return true again, it has to be pressed and released again.

    void Update()
    {
        if (Input.GetMouseButtonUp(0))
        {
            Debug.Log("Left Mouse Button Released");
        }
    
        if (Input.GetMouseButtonUp(1))
        {
            Debug.Log("Right Mouse Button Released");
        }
    
        if (Input.GetMouseButtonUp(2))
        {
            Debug.Log("Middle Mouse Button Released");
        }
    }

## Read Accelerometer Sensor (Basic)
`Input.acceleration` is used to read the accelerometer sensor. It returns `Vector3` as a result which contains `x`,`y` and `z` axis values in 3D space.

<!-- language-all: lang-cs -->
    void Update()
    {
        Vector3 acclerometerValue = rawAccelValue();
        Debug.Log("X: " + acclerometerValue.x + "  Y: " + acclerometerValue.y + "  Z: " + acclerometerValue.z);
    }
    
    Vector3 rawAccelValue()
    {
        return Input.acceleration;
    }



## Read Accelerometer Sensor (Advance)
Using raw values directly from the accelerometer sensor to move or rotate a GameObject can cause problems such as jerky movements or vibrations. It is recommended to smooth out the values before using them. In fact, values from the accelerometer sensor should always be smoothed out before use. This can be accomplished with a low pass filter and this is where `Vector3.Lerp` comes into place.

<!-- language-all: lang-cs -->
    //The lower this value, the less smooth the value is and faster Accel is updated. 30 seems fine for this
    const float updateSpeed = 30.0f;
    
    float AccelerometerUpdateInterval = 1.0f / updateSpeed;
    float LowPassKernelWidthInSeconds = 1.0f;
    float LowPassFilterFactor = 0;
    Vector3 lowPassValue = Vector3.zero;
    
    void Start()
    {
        //Filter Accelerometer
        LowPassFilterFactor = AccelerometerUpdateInterval / LowPassKernelWidthInSeconds;
        lowPassValue = Input.acceleration;
    }
    
    void Update()
    {
    
        //Get Raw Accelerometer values (pass in false to get raw Accelerometer values)
        Vector3 rawAccelValue = filterAccelValue(false);
        Debug.Log("RAW X: " + rawAccelValue.x + "  Y: " + rawAccelValue.y + "  Z: " + rawAccelValue.z);
    
        //Get smoothed Accelerometer values (pass in true to get Filtered Accelerometer values)
        Vector3 filteredAccelValue = filterAccelValue(true);
        Debug.Log("FILTERED X: " + filteredAccelValue.x + "  Y: " + filteredAccelValue.y + "  Z: " + filteredAccelValue.z);
    }
    
    //Filter Accelerometer
    Vector3 filterAccelValue(bool smooth)
    {
        if (smooth)
            lowPassValue = Vector3.Lerp(lowPassValue, Input.acceleration, LowPassFilterFactor);
        else
            lowPassValue = Input.acceleration;
    
        return lowPassValue;
    }

## Read Accelerometer Sensor(Precision)
Read the accelerometer Sensor with precision. 

This example allocates memory:

<!-- language-all: lang-cs -->
    void Update()
    {
        //Get Precise Accelerometer values 
        Vector3 accelValue = preciseAccelValue();
        Debug.Log("PRECISE X: " + accelValue.x + "  Y: " + accelValue.y + "  Z: " + accelValue.z);

    }

    Vector3 preciseAccelValue()
    {
        Vector3 accelResult = Vector3.zero;
        foreach (AccelerationEvent tempAccelEvent in Input.accelerationEvents)
        {
            accelResult = accelResult + (tempAccelEvent.acceleration * tempAccelEvent.deltaTime);
        }
        return accelResult;
    }

This example does **not** allocates memory:

    void Update()
    {
        //Get Precise Accelerometer values 
        Vector3 accelValue = preciseAccelValue();
        Debug.Log("PRECISE X: " + accelValue.x + "  Y: " + accelValue.y + "  Z: " + accelValue.z);

    }

    Vector3 preciseAccelValue()
    {
        Vector3 accelResult = Vector3.zero;
        for (int i = 0; i < Input.accelerationEventCount; ++i)
        {
            AccelerationEvent tempAccelEvent = Input.GetAccelerationEvent(i);
            accelResult = accelResult + (tempAccelEvent.acceleration * tempAccelEvent.deltaTime);
        }
        return accelResult;
    }


Note that this is not filtered. Please look [here][1] for how to smooth accelerometer values to remove noise. 




  [1]: https://www.wikiod.com/unity3d/input-system#Read Accelerometer Sensor (Advance)

