---
title: "Units of Measure"
slug: "units-of-measure"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

# Units at Runtime

Units of Measure are used only for static checking by the compiler, and are not available at runtime. They cannot be used  in reflection or in methods like `ToString`.

For example, C# gives a `double` with no units for a field of type `float<m>` defined by and exposed from an F# library.

## Ensuring Consistent Units in Calculations
Units of measure are additional type annotations that can be added to floats or integers. They can be used to verify at compile time that calculations are using units consistently.

To define annotations:

    [<Measure>] type m // meters
    [<Measure>] type s // seconds
    [<Measure>] type accel = m/s^2 // acceleration defined as meters per second squared

Once defined, annotations can be used to verify that an expression results in the expected type.

    // Compile-time checking that this function will return meters, since (m/s^2) * (s^2) -> m
    // Therefore we know units were used properly in the calculation.
    let freeFallDistance (time:float<s>) : float<m> = 
        0.5 * 9.8<accel> * (time*time)    

    // It is also made explicit at the call site, so we know that the parameter passed should be in seconds
    let dist:float<m> = freeFallDistance 3.0<s>
    printfn "%f" dist



## Conversions between units
    [<Measure>] type m // meters
    [<Measure>] type cm // centimeters

    // Conversion factor
    let cmInM = 100<cm/m>

    let distanceInM = 1<m>
    let distanceInCM = distanceInM * cmInM // 100<cm>

    // Conversion function
    let cmToM (x : int<cm>) = x / 100<cm/m>
    let mToCm (x : int<m>) = x * 100<cm/m>
    
    cmToM 100<cm> // 1<m>
    mToCm 1<m> // 100<cm>
    

Note that the F# compiler does not know that `1<m>` equals `100<cm>`. As far as it cares, the units are separate types. You can write similar functions to convert from meters to kilograms, and the compiler would not care.

    [<Measure>] type kg

    // Valid code, invalid physics
    let kgToM x = x / 100<kg/m>

It is not possible to define units of measure as multiples of other units such as

    // Invalid code
    [<Measure>] type m = 100<cm>

However, to define units "per something", for example Hertz, measuring frequency, is simply "per second", is quite simple.

    // Valid code
    [<Measure>] type s
    [<Measure>] type Hz = /s

    1 / 1<s> = 1 <Hz> // Evaluates to true

    [<Measure>] type N = kg m/s // Newtons, measuring force. Note lack of multiplication sign.

    // Usage
    let mass = 1<kg>
    let distance = 1<m>
    let time = 1<s>

    let force = mass * distance / time // Evaluates to 1<kg m/s>
    force = 1<N> // Evaluates to true


## Unit-of-measure type parameters
The `[<Measure>]` attribute can be used on type parameters to declare types that are generic with respect to units of measure:

    type CylinderSize<[<Measure>] 'u> =
        { Radius : float<'u>
          Height : float<'u> }
          
Test usage:

    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

    /// This has type CylinderSize<m>.
    let testCylinder =
        { Radius = 14.<m>
          Height =  1.<m> }

## Using LanguagePrimitives to preserve or set units
When a function doesn't preserve units automatically due to lower-level operations, the `LanguagePrimitives` module can be used to set units on the primitives that support them:

    /// This cast preserves units, while changing the underlying type
    let inline castDoubleToSingle (x : float<'u>) : float32<'u> =
        LanguagePrimitives.Float32WithMeasure (float32 x)

To assign units of measure to a double-precision floating-point value, simply multiply by one with correct units:

    [<Measure>]
    type USD

    let toMoneyImprecise (amount : float) =
       amount * 1.<USD>

To assign units of measure to a unit-less value that isn't System.Double, for example, arriving from a library written in another language, use a conversion:

    open LanguagePrimitives

    let toMoney amount =
       amount |> DecimalWithMeasure<'u>

Here are the function types reported by F# interactive:

    val toMoney : amount:decimal -> decimal<'u>
    val toMoneyImprecise : amount:float -> float<USD>


## Use standardized unit types to maintain compatibility
For example, types for SI units have been standardized in the F# core library, in `Microsoft.FSharp.Data.UnitSystems.SI`. Open the appropriate sub-namespace, `UnitNames` or `UnitSymbols`, to use them. Or, if only a few SI units are required, they can be imported with type aliases:

    /// Seconds, the SI unit of time. Type abbreviation for the Microsoft standardized type.
    type [<Measure>] s = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.s

Some users tend to do the following, which **should not be done** whenever a definition is already available:

    /// Seconds, the SI unit of time
    type [<Measure>] s // DO NOT DO THIS! THIS IS AN EXAMPLE TO EXPLAIN A PROBLEM.

The difference becomes apparent when interfacing with other code that refers to the standard SI types. Code that refers to the standard units is compatible, while code that defines its own type is incompatible with any code not using its specific definition.

Therefore, always use the standard types for SI units. It doesn't matter whether you refer to `UnitNames` or `UnitSymbols`, since equivalent names within those two refer to the same type:

    open Microsoft.FSharp.Data.UnitSystems.SI

    /// This is valid, since both versions refer to the same authoritative type.
    let validSubtraction = 1.<UnitSymbols.s> - 0.5<UnitNames.second>

