---
title: "XML Documentation Comments"
slug: "xml-documentation-comments"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

Some times you need to **create extended text documentation** from you xml comments. Unfortunatly ***there is no standard way for it***. 

But there are some separate projects that you can use for this case:

 - [Sandcastle][1]
 - [Docu][2]
 - [NDoc][1] 
 - [DocFX][4]


  [1]: http://sandcastle.codeplex.com/
  [2]: http://docu.jagregory.com/
  [3]: http://ndoc.sourceforge.net/
  [4]: https://dotnet.github.io/docfx/

## Simple method annotation
Documentation comments are placed directly above the method or class they describe. They begin with three forward slashes `///`, and allow meta information to be stored via XML.

    /// <summary>
    /// Bar method description
    /// </summary>
    public void Bar()
    { 
            
    }

Information inside the tags can be used by Visual Studio and other tools to provide services such as IntelliSense:

[![Method xml annotation example][1]][1]


  [1]: https://i.stack.imgur.com/NDAnP.png


See also [Microsoft's list of common documentation tags](https://msdn.microsoft.com/en-us/library/5ast78ax.aspx).

## Generating XML from documentation comments
To generate an XML documentation file from documentation comments in the code, use the `/doc` option with the `csc.exe` C# compiler.

In Visual Studio 2013/2015, In **Project** -> **Properties** -> **Build** -> **Output**, check the `XML documentation file` checkbox:

[![XML documentation file][1]][1]

When you build the project, an XML file will be produced by the compiler with a name corresponding to the project name (e.g. `XMLDocumentation.dll` -> `XMLDocumentation.xml`).

When you use the assembly in another project, make sure that the XML file is in the same directory as the DLL being referenced.

This example:

    /// <summary>
    /// Data class description
    /// </summary>
    public class DataClass
    {
        /// <summary>
        /// Name property description
        /// </summary>
        public string Name { get; set; }
    }


    /// <summary>
    /// Foo function
    /// </summary>
    public class Foo
    {
        /// <summary>
        /// This method returning some data
        /// </summary>
        /// <param name="id">Id parameter</param>
        /// <param name="time">Time parameter</param>
        /// <returns>Data will be returned</returns>
        public DataClass GetData(int id, DateTime time)
        {
            return new DataClass();
        }
    }


Produces this xml on build:

    <?xml version="1.0"?>
    <doc>
        <assembly>
            <name>XMLDocumentation</name>
        </assembly>
        <members>
            <member name="T:XMLDocumentation.DataClass">
                <summary>
                Data class description
                </summary>
            </member>
            <member name="P:XMLDocumentation.DataClass.Name">
                <summary>
                Name property description
                </summary>
            </member>
            <member name="T:XMLDocumentation.Foo">
                <summary>
                Foo function
                </summary>
            </member>
            <member name="M:XMLDocumentation.Foo.GetData(System.Int32,System.DateTime)">
                <summary>
                This method returning some data
                </summary>
                <param name="id">Id parameter</param>
                <param name="time">Time parameter</param>
                <returns>Data will be returned</returns>
            </member>
        </members>
    </doc>

  [1]: https://i.stack.imgur.com/tXXQy.png

## Method documentation comment with param and returns elements
    /// <summary>
    /// Returns the data for the specified ID and timestamp.
    /// </summary>
    /// <param name="id">The ID for which to get data. </param>
    /// <param name="time">The DateTime for which to get data. </param>
    /// <returns>A DataClass instance with the result. </returns>
    public DataClass GetData(int id, DateTime time)
    {
       // ...
    }

**IntelliSense** shows you the description for each parameter: 

[![parameter comment][1]][1]

Tip: If Intellisense doesn't display in Visual Studio, delete the first bracket or comma and then type it again.

  [1]: https://i.stack.imgur.com/cH3OQ.png

## Interface and class documentation comments
    /// <summary>
    /// This interface can do Foo
    /// </summary>
    public interface ICanDoFoo
    {
        // ... 
    }

    /// <summary>
    /// This Bar class implements ICanDoFoo interface
    /// </summary>
    public class Bar : ICanDoFoo
    {
        // ...
    }

**Result**

Interface summary

[![interface summary][1]][1]

Class summary

[![class summary][2]][2]

  [1]: https://i.stack.imgur.com/ExpwI.png
  [2]: https://i.stack.imgur.com/730eY.png

## Referencing another class in documentation
The `<see>` tag can be used to link to another class. It contains the `cref` member which should contain the name of the class that is to be referenced. Visual Studio will provide Intellsense when writing this tag and such references will be processed when renaming the referenced class, too.

    /// <summary>
    /// You might also want to check out <see cref="SomeOtherClass"/>.
    /// </summary>
    public class SomeClass
    {
    }
In Visual Studio Intellisense popups such references will also be displayed colored in the text.

To reference a generic class, use something similar to the following:

    /// <summary>
    /// An enhanced version of <see cref="List{T}"/>.
    /// </summary>
    public class SomeGenericClass<T>
    {
    }

