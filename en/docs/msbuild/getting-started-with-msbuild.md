---
title: "Getting started with msbuild"
slug: "getting-started-with-msbuild"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
 
HelloWorld.proj  
 
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003" DefaultTargets="SayHello">  

        <!-- Properties can be passed as command line parameters. i.e. /p:Name=MyName 
        or /p:Name="My Name" (Use quotes if the value includes spaces) -->  
        <PropertyGroup>
            <Name Condition="'$(Name)'==''">Rolo</Name>
        </PropertyGroup>  
    
        <!-- Items can't be passed as command line parameters. -->   
        <!-- Items can include metadata. i.e. URL -->  
        <ItemGroup>  
            <People Include="World"/>  
            <People Include="StackOverflow">  
                <URL>http://stackoverflow.com</URL>
            </People>
            <People Include="Google">  
                <URL>http://google.com</URL>  
            </People>  
        </ItemGroup>  
 
        <!-- Targets can be called using it's name. i.e. /t:SayHello -->  
        <Target Name="SayHello">  
            <!-- You can have as many Tasks as required inside a Target. -->  
            <!-- Tasks can be executed conditionally. -->  
            <Message Condition="'%(People.URL)'==''" Text="Hello %(People.Identity), my name is $(Name)! "/>  
            <Message Condition="'%(People.URL)'!=''" Text="Hello %(People.Identity), my name is $(Name)!. Your URL is %(People.URL) "/>  
        </Target>  
    </Project>  
 
**Execute with:**  
  
 - msbuild HelloWorld.proj  
 - msbuild HelloWorld.proj /p:Name="John Doe"   
 - msbuild HelloWorld.proj /p:Name="Batman" /t:SayHello 

## Installation or Setup
## MSBuild 2015
On Windows there are three choices to get MSBuild:  
* Install Visual Studio 2015
* Download [Microsoft Build Tools][1] which includes VB and C# compilers.
* Build from [Source][2]

On Linux 
* Build from Source using [this guide][3]


  [1]: https://www.microsoft.com/en-us/download/details.aspx?id=48159
  [2]: https://github.com/Microsoft/msbuild
  [3]: https://github.com/Microsoft/msbuild/wiki/Building-Testing-and-Debugging-on-.Net-Core-MSBuild

## Creating Custom MSBuild Targets
    <PropertyGroup>
         <!-- Definition of a Property named "TestCondition". A PropertyGroup may also be placed inside a Target. -->
        <TestCondition>True</TestCondition>
    </PropertyGroup>

    <!-- This Target will run after the "Clean" Target, subject to a Condition. -->
    <Target Name="SpecificTarget" AfterTargets="Clean" Condition=" '$(TestCondition)' == 'True' ">
        <!-- Displaying a custom message -->
        <Message Text="Here is my Specific Target" Importance="Low" />
        <!-- Here come your specific code. -->
    </Target>


