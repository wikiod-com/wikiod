---
title: "Building a Jekyll site folder"
slug: "building-a-jekyll-site-folder"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Syntax
- jekyll build [flag] [value] # Build the site with the setting specified by &lt;flag&gt; and &lt;value&gt;
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
\# cf. [list of available settings for Jekyll][1]


[1]: https://jekyllrb.com/docs/configuration/#configuration-settings

If you edit `_config.yml` and you are using `--watch`, you need to restart the command to apply the changes.

## Building the site to a folder
````
$ jekyll build
# The current site folder will be built into the ./_site directory

$ jekyll build --destination /var/www/
# The current site folder will be generated into /var/www/

$ jekyll build --watch
# The current site folder will be built into the ./_site directory and will be kept up to date with the source until you press CTRL+C to kill the process
````

## Building with a specific Jekyll environment
you can set a Jekyll environment and value, when build time

```
JEKYLL_ENV=production jekyll b
```

```
JEKYLL_ENV=production jekyll build
```

```
JEKYLL_ENV=production bundle exec jekyll build
```

---

if your code contains the bellow snippet, `analytics.html` will not be included unless your building with `JEKYLL_ENV=production`

```liquid
{% if jekyll.environment == "production" %}

   {% include analytics.html %}

{% endif %}
```

