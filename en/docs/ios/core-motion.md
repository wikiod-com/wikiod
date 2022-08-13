---
title: "Core Motion"
slug: "core-motion"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Accessing Barometer to get relative altitude
**Swift**

Import the Core Motion library:

    import CoreMotion

Next, we need to create a `CMAltimeter` object, but a common pitfall is to create it in the `viewDidLoad()`. If done that way, the altimeter won’t be accessible when we need to call a method on it. Nevertheless, go ahead and create your `CMAltimeter` object just before the `viewDidLoad()`:

    let altimeter = CMAltimeter()

**Now:**
 1. We need to check if `relativeAltitude` is even available with the
    following method: `CMAltimeter.isRelativeAltitudeAvailable`.
 2. If that returns `true`, you can then begin monitoring altitude change with `startRelativeAltitudeUpdatesToQueue`

 3. If there are no errors, you should be able to retrieve data from the `relativeAltitude` and pressure properties.

Given below is the definition of a button action to begin monitoring with our barometer.

    @IBAction func start(sender: AnyObject){
    if CMAltimeter.isRelativeAltitudeAvailable() {
        // 2
        altimeter.startRelativeAltitudeUpdatesToQueue(NSOperationQueue.mainQueue(), withHandler: { data, error in
            // 3
            if (error == nil) {
                println("Relative Altitude: \(data.relativeAltitude)")
                println("Pressure: \(data.pressure)")
            }
        })
    }

