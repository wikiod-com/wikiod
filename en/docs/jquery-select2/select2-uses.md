---
title: "Select2 Uses"
slug: "select2-uses"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Select never work in Bootstrap Modal follow given steps to use select2 in Bootstrap Model.

## Using Select2 in a Bootstrap Modal/PopUp
If you are using `Bootstrap Modal` then be sure that  Model tabindex=-1  is removed.  

     $('#targetId').select2({
            width: '100%',
            dropdownParent: $("#myModal")
        })

## Using Select2 in a Bootstrap Modal/PopUp
Using `Select2` in a Bootstrap Modal/PopUp
If you are using Bootstrap Modal then be sure that Model tabindex=-1 is removed.

    $('#targetId').select2({ width: '100%', dropdownParent: $("#myModal") })

