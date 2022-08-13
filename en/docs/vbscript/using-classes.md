---
title: "Using Classes"
slug: "using-classes"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Creating a Class
    Class Car
    
        Private wheels_
        Private distances_
    
        ' Property getter
        Public Property Get Wheels()
            Wheels = wheels_
        End Property
        
        ' Property setter
        Public Property Let Wheels(v)
            wheels_ = v
        End Property
        
        ' Parameterless Constructor
        Public Sub Class_Initialize()
            distances_ = Array(0)
        End Sub
        
        ' Method
        Public Function GetTotalDistance()
            dim d
            'GetTotalDistance = 0
            For Each d in distances_
                GetTotalDistance = GetTotalDistance + d
            Next
        End Function
        
        ' Void Method
        Public Sub Drive(distance)
            distances_(ubound(distances_)) = distance
            Redim Preserve distances_(ubound(distances_)+1)
        End Sub
        
    End Class


## Using a Class Instance
    ' Initialize the object
    Dim myCar
    Set myCar = new Car
    
    ' Setting a property
    myCar.Wheels = 4
    
    ' Getting a property value
    wscript.echo myCar.Wheels
    
    ' Using a subroutine in a class
    myCar.Drive 10
    myCar.Drive 12
    
    ' Using a function in a class
    wscript.echo myCar.GetTotalDistance()    ' returns 22

## Global Factory Function to Emulate a Parameterized Constructor
    ' Making a factory with parameter to the class
    Public Function new_Car(wheels)
        Set new_Car = New Car
        new_Car.Wheels = wheels
    End Function
    
    ' Creating a car through a factory
    Dim semiTrailer
    Set semiTrailer = new_Car(18)

## Init Method to Emulate a Parameterized Constructor
    Class Car
        ...
        ' Parameterless Constructor
        Public Sub Class_Initialize()
            distances_ = Array(0)
        End Sub

        ' Default initialization method that can be invoked without
        ' explicitly using the method name.
        Public Default Function Init(wheels)
            wheels_ = wheels
            Set Init = Me
        End Function
        ...
    End Class

    Set car1 = (New Car)(18)       ' implicit invocation
    Set car2 = (New Car).Init(8)   ' explicit invocation

## Loading external Class files into script.
    Dim classFile : classFile = "carClass.vbs"
    Dim fsObj : Set fsObj = CreateObject("Scripting.FileSystemObject")
    Dim vbsFile : Set vbsFile = fsObj.OpenTextFile(classFile, 1, False)
    Dim myFunctionsStr : myFunctionsStr = vbsFile.ReadAll
    vbsFile.Close
    Set vbsFile = Nothing
    Set fsObj = Nothing
    ExecuteGlobal myFunctionsStr
    
    Dim car1 : Set car1 = (New Car)(18)

