---
title: "Add custom class for links in RTE"
slug: "add-custom-class-for-links-in-rte"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Here is a quick example to see how it is possible to add custom classes for links in TYPO3 RTE.



## Add custom link classes

```
RTE {
    classes {
        btn-lg {
            name = Button (large)
            requires = btn btn-default
        }

        btn-default {
            name = Button (default)
            requires = btn
        }
    }

    default {
        buttons.link.properties.class.allowedClasses := addToList(btn-lg, btn-default)
        proc.allowedClasses := addToList(btn-lg, btn-default)
    }
}
```

