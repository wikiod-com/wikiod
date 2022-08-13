---
title: "Generación de código T4"
slug: "generacion-de-codigo-t4"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Sintaxis
- **Sintaxis T4**
- `<#@...#>` //Declaración de propiedades, incluidas plantillas, ensamblados y espacios de nombres y el idioma que usa la plantilla
- `Texto sin formato` //Declaración de texto que se puede recorrer en bucle para los archivos generados
- `<#=...#>` //Declaración de secuencias de comandos
- `<#+...#>` //Declarando scriptlets
- `<#...#>` //Declaración de bloques de texto

## Generación de código en tiempo de ejecución
    <#@ template language="C#" #> //Language of your project 
    <#@ assembly name="System.Core" #>
    <#@ import namespace="System.Linq" #>
    <#@ import namespace="System.Text" #>
    <#@ import namespace="System.Collections.Generic" #>


