---
title: "T4 Kod Oluşturma"
slug: "t4-kod-olusturma"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Sözdizimi
- **T4 Sözdizimi**
- `<#@...#>` //Şablonlar, derlemeler ve ad alanları dahil özellikleri ve şablonun kullandığı dili bildirme
- `Düz Metin` //Oluşturulan dosyalar için döngüye girebilecek metin bildiriliyor
- `<#=...#>` //Komut Dosyalarını Bildirme
- `<#+...#>` //Scriptlet bildiriliyor
- `<#...#>` //Metin blokları bildiriliyor

## Çalışma Zamanı Kodu Oluşturma
    <#@ template language="C#" #> //Language of your project 
    <#@ assembly name="System.Core" #>
    <#@ import namespace="System.Linq" #>
    <#@ import namespace="System.Text" #>
    <#@ import namespace="System.Collections.Generic" #>


