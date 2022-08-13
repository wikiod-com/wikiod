---
title: "Meta Documentation Guidelines"
slug: "meta-documentation-guidelines"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This is an extension of [Python's "Meta: Documentation Guidelines"][1] for Django.

These are just proposals, not recommendations. Feel free to edit anything here if you disagree or have something else to mention.

[1]: https://www.wikiod.com/python

## Unsupported versions don't need special mention
It is unlikely that someone uses an unsupported version of Django, and at his own risks. If ever someone does, it must be his concern to know if a feature exists in the given version.

Considering the above, it is useless to mention specificities of an unsupported version.

<!-- if version [lt 1.6] -->
This kind of block is useless because no sane person uses Django < 1.6.
<!-- end version if -->

<!-- if version [gte 1.8] -->
This kind of block is useless because no sane person uses Django < 1.8.
<!-- end version if -->

This also goes for topics. At the time of writing this example, [Class based views][1] states supported versions are `1.3-1.9`. We can safely assume this is actually equivalent to `All versions`. This also avoids upgrading all topics supported versions every time a new version is released.

> Current supported versions are: **`1.8`<sup>1</sup> `1.9`<sup>2</sup> `1.10`<sup>1</sup>**

1. Security fixes, data loss bugs, crashing bugs, major functionality bugs in newly-introduced features, and regressions from older versions of Django.
2. Security fixes and data loss bugs.

[1]: https://www.wikiod.com/django/class-based-views

