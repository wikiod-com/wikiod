---
title: "Merged Cells  Ranges"
slug: "merged-cells--ranges"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Think twice before using Merged Cells/Ranges
First of all, Merged Cells are there only to improve the look of your sheets.

So it is literally the last thing that you should do, once your sheet and workbook are totally functional!


----------


Where is the data in a Merged Range?
------------------------------------

When you merge a Range, you'll only display one block.

The data will be in the very **first cell of that Range**, and the **others will be empty cells**!

One good point about it : no need to fill all the cells or the range once merged, just fill the first cell! ;)

The other aspects of this merged ranged are globally negative :

- If you use a [method for finding last row or column][1], you'll risk some errors

- If you loop through rows and you have merged some ranges for a better readability, you'll encounter empty cells  and not the value displayed by the merged range


  [1]: https://www.wikiod.com/excel-vba/methods-for-finding-the-last-used-row-or-column-in-a-worksheet

