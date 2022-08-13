---
title: "Templates"
slug: "templates"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Get Sitecore Template Item by ID
To get template item by template Id.

    TemplateItem templateItem = Sitecore.Context.Database.GetTemplate(new ID("{11111111-1111-1111-1111-111111111111}"));

## Get Sitecore Template Item by name
Input the name of the template to get the template by using GetTemplate Method.

    TemplateItem templateItem = Sitecore.Context.Database.GetTemplate("NameOfTheTemplate");

