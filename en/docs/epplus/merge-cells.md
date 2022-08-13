---
title: "Merge Cells"
slug: "merge-cells"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

How to merge cells

## Merging cells
<!-- language: lang-c# -->

    //By range address
    worksheet.Cells["A1:B5"].Merge = true;

    //By indexes
    worksheet.Cells[1,1,5,2].Merge = true;

