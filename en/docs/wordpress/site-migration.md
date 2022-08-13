---
title: "Site Migration"
slug: "site-migration"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Syntax
- OLD_SITE - The old site being migrated (eg: http://localhost/example)
- NEW_SITE - The new site to which to migrate (eg: https://example.com

## Updating the tables with a new URL
    UPDATE wp_options SET option_value = replace(option_value, 'OLD_SITE, 'NEW_SITE') WHERE option_name = 'home' OR option_name = 'siteurl';
    UPDATE wp_posts SET guid = replace(guid, 'OLD_SITE','NEW_SITE');
    UPDATE wp_posts SET post_content = replace(post_content, 'OLD_SITE', 'NEW_SITE');

