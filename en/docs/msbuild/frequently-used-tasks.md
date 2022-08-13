---
title: "Frequently-used Tasks"
slug: "frequently-used-tasks"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Copying files
    <ItemGroup>
         <DataToCopy Include="*.cs;*.aspx" />
    </ItemGroup>
    <Copy SourceFiles="@(DataToCopy)" DestinationFolder="SourceCopiedFolder" />


## Deleting files
    <ItemGroup>
        <FilesToDelete Include="*.tmp" />
    </ItemGroup>

    <Delete Files="@(FilesToDelete)" />


## Creating a new directory
    <PropertyGroup>
        <DirectoryToCreate>NewDirectory</DirectoryToCreate>
    </PropertyGroup>
    <MakeDir Directories="$(DirectoryToCreate)" />


## Removing an existing directory
    <PropertyGroup>
        <DirectoryToRemove>TempData</DirectoryToRemove>
    </PropertyGroup>
    <RemoveDir Directories="$(DirectoryToRemove)" />


## Running a custom command
    <Exec Command="echo Hello World" />


## Displaying a custom message
    <PropertyGroup>
        <CustomMessage>Hello World</CustomMessage>
        <MessageImportance>Low</MessageImportance> <!-- Low / Normal / High -->
    </PropertyGroup>
    <Message Text="$(CustomMessage)" Importance="$(MessageImportance)" />


## Running MSBuild on another project / solution
    <PropertyGroup>
        <LinkedSolution>LinkedSolution.sln</LinkedSolution>
        <BuildType>Build</BuildType> <!-- Build / Rebuild -->
        <BuildArchitecture>x86</BuildArchitecture> <!-- x86 / 64 -->
        <BuildConfiguration>Debug</BuildConfiguration> <!-- Debug / Release -->
    </PropertyGroup>
    <MSBuild Projects="$(LinkedSolution)"
             Targets="$(BuildType)" 
             Properties="Architecture=$(BuildArchitecture);Configuration=$(BuildConfiguration)" />

