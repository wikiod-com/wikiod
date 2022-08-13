---
title: "Génération de codes T4"
slug: "generation-de-codes-t4"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Syntaxe
- **Syntaxe T4**
- `<#@...#>` //Déclarer les propriétés, y compris les modèles, les assemblages et les espaces de noms, ainsi que le langage utilisé par le modèle
- `Plain Text` //Déclaration de texte pouvant être parcouru en boucle pour les fichiers générés
- `<#=...#>` //Déclarer des scripts
- `<#+...#>` //Déclaration de scriptlets
- `<#...#>` //Déclarer des blocs de texte

## Génération de code d'exécution
    <#@ template language="C#" #> //Language of your project 
    <#@ assembly name="System.Core" #>
    <#@ import namespace="System.Linq" #>
    <#@ import namespace="System.Text" #>
    <#@ import namespace="System.Collections.Generic" #>


