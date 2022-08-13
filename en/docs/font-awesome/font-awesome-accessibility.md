---
title: "Font Awesome & Accessibility"
slug: "font-awesome--accessibility"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Icons used for pure decoration or visual styling
If you're using an icon to add some extra decoration or branding, it does not need to be announced to users as they are navigating your site or app aurally. Additionally, if you're using an icon to visually re-emphasize or add styling to content already present in your HTML, it does not need to be repeated to an assistive technology-using user. You can make sure this is not read by adding the aria-hidden="true" to your Font Awesome markup.

    <i class="fa fa-fighter-jet" aria-hidden="true"></i>



