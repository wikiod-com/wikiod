---
title: "Order of Target Execution"
slug: "order-of-target-execution"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

From MSDN: [Target Build Order][1]

> # Determining the Target Build Order # 
> MSBuild determines the target build order as follows:
>  1. InitialTargets targets are run.
>  2. Targets specified on the command line by the /target switch are run.
>     If you specify no targets on the command line, then the
>     DefaultTargets targets are run. If neither is present, then the
>     first target encountered is run.
>  2. The Condition attribute of the target is evaluated. If the Condition
>     attribute is present and evaluates to false, the target isn't
>     executed and has no further effect on the build.
>  2. Before a target is executed, its DependsOnTargets targets are run.    
>  2. Before a target is executed, any target that lists it in a
>     BeforeTargets attribute is run.
>  2. Before a target is executed, its Inputs attribute and Outputs
>     attribute are compared. If MSBuild determines that any output files
>     are out of date with respect to the corresponding input file or
>     files, then MSBuild executes the target. Otherwise, MSBuild skips
>     the target.
>  2. After a target is executed or skipped, any target that lists it in
>     an AfterTargets attribute is run.

  [1]: https://msdn.microsoft.com/en-us/library/ee216359.aspx

## DependsOnTargets
Define a sequence of Targets (`Target1`, then `Target2`) that must execute before `Target3`. Note that an execution request for `Target3` is required to cause `Target1` and `Target2` to be executed.

    <Target Name="Target3" DependsOnTargets="Target1;Target2">
    </Target>

    <Target Name="Target2">
    </Target> 
    
    <Target Name="Target1">
    </Target> 


## AfterTargets
Define a Target (`Target1`) for which an execution request will cause `Target2` to be executed afterward.

    <Target Name="Target2" AfterTargets="Target1">
    </Target> 
    
    <Target Name="Target1">
    </Target> 


## BeforeTargets
Define a Target (`Target2`) for which an execution request will cause `Target1` to be executed beforehand.

    <Target Name="Target2">
    </Target> 
    
    <Target Name="Target1" BeforeTargets="Target2">
    </Target> 

