---
title: "Order of Property and Item Evaluation"
slug: "order-of-property-and-item-evaluation"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

For more detail, see [Property and Item Evaluation Order][1] on the MSDN documentation page *Comparing Properties and Items*.


  [1]: https://msdn.microsoft.com/en-us/library/dd997067.aspx#Anchor_2

## Example illustrating the order of evaluation
MSBuild evaluates `PropertyGroup`, `Choose` and `ItemGroup` elements that are directly under the `Project` element before those that are in `Target` elements.
* Directly under the `Project` element, `PropertyGroup` and `Choose` elements are evaluated in the order in which they appear, and then `ItemGroup` elements are evaluated in the order in which they appear.
* In `Target` elements `PropertyGroup` and `ItemGroup` share equal precedence and are evaluated in the order in which they appear.

Within files referenced via `Import`, MSBuild evaluates `PropertyGroup`, `Choose` and `ItemGroup` in the same manner as above, and as though the imported files' content appeared inline where the `Import` is located.

The comments below provide property values and item counts before and after MSBuild evaluates selected lines.

    <Project DefaultTargets="FooTarget" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
        <Target Name="FooTarget">
            <ItemGroup>
                <!-- '$(FooProp)' == '2', '@(FooItem->Count())' == '1' -->
                <FooItem Include="foo value B" />
                <!-- '$(FooProp)' == '2', '@(FooItem->Count())' == '2' -->
            </ItemGroup>
            <PropertyGroup>
                <!-- '$(FooProp)' == '2', '@(FooItem->Count())' == '2' -->
                <FooProp>3</FooProp>
                <!-- '$(FooProp)' == '3', '@(FooItem->Count())' == '2' -->
            </PropertyGroup>
        </Target>
        <ItemGroup>
            <!-- '$(FooProp)' == '2', '@(FooItem->Count())' == '0' -->
            <FooItem Include="foo value A" />
            <!-- '$(FooProp)' == '2', '@(FooItem->Count())' == '1' -->
        </ItemGroup>
        <PropertyGroup>
            <!-- '$(FooProp)' == '', '@(FooItem->Count())' == '0' -->
            <FooProp>1</FooProp>
            <!-- '$(FooProp)' == '1', '@(FooItem->Count())' == '0' -->
        </PropertyGroup>
        <Choose>
            <When Condition=" '$(FooProp)' == '1' ">
                <!-- '$(FooProp)' == '1', '@(FooItem->Count())' == '0' -->
                <FooProp>2</FooProp>
                <!-- '$(FooProp)' == '2', '@(FooItem->Count())' == '0' -->
            </When>
        </Choose>
    </Project>

