---
title: "Geração de código T4"
slug: "geracao-de-codigo-t4"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Sintaxe
- **Sintaxe T4**
- `<#@...#>` //Declarando propriedades incluindo templates, assemblies e namespaces e a linguagem que o template usa
- `Plain Text` //Declarando o texto que pode ser repetido para os arquivos gerados
- `<#=...#>` //Declarando scripts
- `<#+...#>` //Declarando scriptlets
- `<#...#>` //Declarando blocos de texto

## Geração de código de tempo de execução
    <#@ template language="C#" #> //Language of your project 
    <#@ assembly name="System.Core" #>
    <#@ import namespace="System.Linq" #>
    <#@ import namespace="System.Text" #>
    <#@ import namespace="System.Collections.Generic" #>


