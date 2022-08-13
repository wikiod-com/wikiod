---
title: "Common Item Types ProjectReference"
slug: "common-item-types-projectreference"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

A `ProjectReference` defines a reference to another project.

## Parameters
| Parameter | Details |
| --------- | ------- |
| `Include` (attribute) | Path to project file |
| `Project` (metadata) | Project GUID, in the form {00000000-0000-0000-0000-000000000000} |
| `ReferenceOutputAssembly` (metadata) | Boolean specifying whether the outputs of the project referenced should be passed to the compiler. Default is true. |
| `SpecificVersion` (metadata) | Whether the exact version of the assembly should be used. |
| `Targets` (metadata) | Semicolon-separated list of targets in the referenced projects that should be built. Default is the value of `$(ProjectReferenceBuildTargets)` whose default is blank, indicating the default targets. |
| `OutputItemType` (metadata) | Item type to emit target outputs into. Default is blank. If `ReferenceOutputAssembly` is set to "true" (default) then target outputs will become references for the compiler. |
| `EmbedInteropTypes` (metadata) | Optional boolean. Whether the types in this reference need to embedded into the target assembly - interop asemblies only |

When the `OutputItemType` parameter is used, additional parameters (metadata) may be applicable. For example, when `OutputItemType` is set to `Content`, `CopyToOutputDirectory` can be used.

| Parameter | Details |
| --------- | ------- |
| `CopyToOutputDirectory` (metadata) | Optional string. Determines whether to copy the file to the output directory. Values: `Never`, `Always`, `PreserveNewest`. |

## Simple ProjectReference
      <ItemGroup>
        <ProjectReference Include="Foo.csproj">
          <Project>{01234567-0123-0123-0123-0123456789AB}</Project>
          <Name>Foo</Name>
        </ProjectReference>
      </ItemGroup>


