---
title: "Bootstrap Themes"
slug: "bootstrap-themes"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Bootstrap themes versus rule overrides
**What are themes?**

There are several visual appearance out there for Bootstrap, which can be found from sources, such as [Bootswatch][1], which are modifying the *bootstrap.min.css* file. You can also create your own theme this way.

**When to modify themes and when to add new rules to a site.css file?**

When to modify the 

> bootstrap.min.css

 file, and when to add your own .css file, such as

> site.css

?

Sometimes there are style requirements, which must be done, no matter what theme you are using. These rules should go into your own .css file, such as *site.css*, so the main theme can be changed, the rules from *site.css* will apply anyhow. In order to do that, you just have to link the bootstrap theme, and your own rules, to override the existing ones:

    <link href="../Content/bootstrap.min.css" rel="stylesheet">
    <link href="../Content/site.css" rel="stylesheet">

This way, Bootstrap themes can be changed anytime without losing the mandatory rules, applied from *site.css*.

   [1]: http://bootswatch.com/

