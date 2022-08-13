---
title: "T4 Code Generation"
slug: "t4-code-generation"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Syntax
- **T4 Syntax**
- `<#@...#>` //Declaring properties including templates, assemblies and namespaces and the language the template uses
- `Plain Text` //Declaring text that can be looped through for the files generated
- `<#=...#>` //Declaring Scripts 
- `<#+...#>` //Declaring scriptlets 
- `<#...#>` //Declaring text blocks

## Runtime Code Generation
    <#@ template language="C#" #> //Language of your project 
    <#@ assembly name="System.Core" #>
    <#@ import namespace="System.Linq" #>
    <#@ import namespace="System.Text" #>
    <#@ import namespace="System.Collections.Generic" #>


