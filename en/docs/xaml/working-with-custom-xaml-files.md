---
title: "Working with custom XAML files"
slug: "working-with-custom-xaml-files"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Reading an object from XAML
<!-- language-all: c# -->
Consider a structure of the following classes should be constructed in XAML an then read into a CLR object:

    namespace CustomXaml
    {
        public class Test
        {
            public string Value { get; set; }
            public List<TestChild> Children { get; set; } = new List<TestChild>(); 
        }
    
        public class TestChild
        {
            public string StringValue { get; set; }
            public int IntValue { get; set; }
        }
    }

Classes should either have no explicit constructor or provide an empty one. To keep the XAML clean, collections need to be initialised. Initialising collections in XAML is also possible though.

To read XAML the `XamlServices` class can be used. It is defined in `System.Xaml` which needs to be added to references. The following line then reads the `test.xaml` file from disk:

    Test test = XamlServices.Load("test.xaml") as Test;

The `XamlServices.Load` method has several overloads to load from streams and other sources. If reading XAML from an embedded file (like it is done in WPF) the `Build Action` property that is set to `Page` by default needs to be changed to i.e. `Embedded Resource`. Otherwise the compiler will ask for references to WPF assemblies.

The content of the XAML file to read should look something like this:

    <Test xmlns="clr-namespace:CustomXaml;assembly=CustomXaml"
          xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
          Value="test">
        <Test.Children>
            <TestChild StringValue="abc" IntValue="123"/>
            <TestChild StringValue="{x:Null}" IntValue="456"/>
        </Test.Children>
    </Test>

The pure `xmlns` Definition allows the use of classes in the same namespace without prefix. The Definition of the `xmlns:x` is neccessary to use constructs like `{x:Null}`. Of course prefixes for other namespaces or assemblies can be defined as needed.




## Writing an object to XAML
<!-- language-all: c# -->
Consider a structure of the following classes should be constructed in XAML an then read into a CLR object:

    namespace CustomXaml
    {
        public class Test
        {
            public string Value { get; set; }
            public List<TestChild> Children { get; set; } = new List<TestChild>(); 
        }
        
        public class TestChild
        {
            public string StringValue { get; set; }
            public int IntValue { get; set; }
        }
    }

To write XAML the `XamlServices` class can be used. It is defined in `System.Xaml` which needs to be added to references. The following line then writes the instance `test` which is of type `Test` to the file `test.xaml` on disk:

    XamlServices.Save("test.xaml", test);

The `XamlServices.Save` method has several overloads to write to streams and other targets.
The resulting XAML should look something like this:

    <Test Value="test" xmlns="clr-namespace:CustomXaml;assembly=CustomXaml"
                       xmlns:scg="clr-namespace:System.Collections.Generic;assembly=mscorlib"
                       xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml">
      <Test.Children>
        <scg:List x:TypeArguments="TestChild" Capacity="4">
          <TestChild IntValue="123" StringValue="abc" />
          <TestChild IntValue="456" StringValue="{x:Null}" />
        </scg:List>
      </Test.Children>
    </Test>
The pure `xmlns` Definition allows the use of classes in the same namespace without prefix. The Definition of the `xmlns:x` is neccessary to use constructs like `{x:Null}`. The writer automatically adds the `xmlns:scg` to initialize a `List<TestChild>` for the `Children` property of the `Test` object. It does not rely on the property being initialized by the constructor.


