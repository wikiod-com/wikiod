---
title: "Sitecore query syntax"
slug: "sitecore-query-syntax"
draft: false
images: []
weight: 9888
type: docs
toc: true
---

**Template Names vs. Template IDs vs. Item Names in Queries:**

I strongly recommend that you *use Template IDs and **not** Template Names or Item Names* in your queries. This will ensure that your queries will still work, even when templates and/or items are renamed. 

The one exception to this is when working with OOTB templates, while querying an OOTB structure, e.g. `/sitecore/content` or `/sitecore/system/Marketing Control Panel`. In these situations, the loss of readability is often greater than the risk of queries breaking, since these templates are far less likely to be renamed.

Note that template names were used in my examples, above, for sake of readabiltiy. Those queries should not be used in production, unless the template names are replaced with template IDs. 


**Reference Sheet:**

I noticed that the Sitecore Query Cheat Sheet is no longer available for download on the web (all of the Sitecore-hosted links now redirect to 404 pages). Fortunately, I had a copy on my machine, and have added a screenshot, below: 

[![Sitecore Query Cheat Sheet][1]][1]


  [1]: http://i.stack.imgur.com/GWePd.png

## Site-specific query
**Tree structure:**

    /sitecore
        /content
            /foo-site
                /home
                    /my-account
            /bar-site
                /home
                    /my-account
            /baz-site
                /home
                    /my-account

 - The template of each site item (`foo-site`, `bar-site`, `baz-site`) is named `Site Node`.
 - The template of each home item (`home`, `home`, `home`) is named `Homepage`
 - The template of each user account item (`my-account`, `my-account`, `my-account`) is named `User Account Page`

**Current Item:**

The current item  could be the `home` item or any page underneath the `home` item for any of the given sites, and this query will still work (provided that there are no items with the `Homepage` template underneath the `home` items that are an ancestor of the current item).

**Query:**

    query:./ancestor-or-self::*[@@templatename='Homepage']/*[@@templatename='my-account']

**Result:**

If querying from the `home` item or one of its descendants in the `foo-site` site:

    /sitecore/content/foo-site/home/my-account

If querying from the `home` item or one of its descendants in the `bar-site` site:

    /sitecore/content/bar-site/home/my-account

If querying from the `home` item or one of its descendants in the `baz-site` site:

    /sitecore/content/baz-site/home/my-account

## Select by item path
**Query:**

    query:/sitecore/content/home/foo/bar

**Result**

    bar



## Relative query
**Current Item:**

    bar (path: /sitecore/content/home/foo/bar)

**Query:**

    query:./child/grandchild

**Result:**

    grandchild (path: /sitecore/content/home/foo/bar/child/grandchild)

## Item attributes query
**Query:**

    query:/sitecore/content/[@@templatename='Homepage']

**Result:**

    home (name: home, path: /sitecore/content/home, template name: Homepage)

