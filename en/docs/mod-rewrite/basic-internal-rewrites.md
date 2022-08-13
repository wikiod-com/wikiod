---
title: "Basic internal rewrites"
slug: "basic-internal-rewrites"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Fancy url to php script
In this example, we rewrite url's of the form `http://example.com/topic/id-seoname` to a php script that takes an id as input. This example expects the rule to be in ["per-directory" context](https://www.wikiod.com/mod-rewrite/contexts-of-rewrite-rules#Rewrite rules in per-directory context).

    RewriteEngine on

    RewriteRule ^topic/([0-9]+)-[^/]*/?$ /topics.php?id=$1 [L]

In this example, `topic/` is the common prefix of all topics. It is followed by a number that is used by the script. Lastly, the seo name is displayed. This seo name is ignored by mod_rewrite, because it is only there for seo reasons. The second argument of `RewriteRule` contains the url to rewrite to. The placeholder `$1` is replaced with the content of the first capture group in the regex before it. In this case it will be replaced with what is matched with `([0-9]+)`.

## Url with query string to php script
To match a query string, a condition must be added to the `RewriteRule`. This is done by putting `RewriteCond` directives before the corresponding rule. In the following example we dynamically internally rewrite an old url to a new url.

    RewriteCond %{QUERY_STRING} ^name=([^&]*)$
    RewriteRule ^oldscript\.php$ newscript.php?username=%1 [L]

Please note that to match the literal dot, we have to escape it with a slash. `%1` is replaced with the first capture group of the previous condition. In this case it is replaced by whatever is matched by `([^&]*)`.

